import constants
import asyncio
import time
import sys
import re
import csv
import datetime
import traceback
import aiohttp
from time import sleep
from constants import *
from ossapi import OssapiAsync, UserLookupKey, GameMode, RankingType, MatchEventType, BeatmapsetCompact, Ossapi
from ossapi import User

api = OssapiAsync(CLIENT_ID, API_KEY)
aapi = Ossapi(CLIENT_ID, API_KEY)

# a tournament match is defined as a multiplayer match whose name fits the format
#   name: (team 1) vs (team 2)
tourney_re = re.compile(r".*: \(.*\) [vV][sS]?\.? \(.*\)")

async def scrape_all(mod, start, end, overwrite = False):
    start_id = start
    end_id =   end
    dest = f"csvfiles/mp_data/all_mps_a{mod}.csv"
    logdest = f"csvfiles/mp_data/id_abbrs_osu_{mod}.csv"
    if overwrite: 
        with open(dest, "w", newline='') as f:
            writer = csv.writer(f)
            writer.writerow(("MatchID", 
                        "TourneyAbbreviation",
                        "Datetime", 
                        "MapID", 
                        "UserID", 
                        "Username", 
                        "Mods", 
                        "Score"))
    prev_time = datetime.datetime.now()
    for i in range(start_id, end_id):
        if not (i%7 == mod):
            continue
        for attempt in range(10):
            try:
                match_response = await api.match(i)
                match_name = match_response.match.name
                valid_match = re.fullmatch(tourney_re, match_name) != None
                if not valid_match: break
                if (i%100 == 0): print(f"match id {i} success")

                abbr = (match_name[0:match_name.find(":")]).strip()
                await write_csv((abbr, i), logdest)

                users = match_response.users
                user_dict = dict()
                for user in users:
                    user_dict[user.id] = user.username
                # gather all of the maps played in this match
                fid = match_response.first_event_id
                events = match_response.events
                lowest = 999999999999
                all_events = list()
                for event in events:
                    lowest = min(lowest, event.id)
                    if event.detail.type == MatchEventType.OTHER:
                        all_events.append(event)
                while lowest > fid:
                    new_match_response = aapi.match(i, before=lowest)
                    for user in new_match_response.users:
                        user_dict[user.id] = user.username
                    more_events = new_match_response.events
                    for event in more_events:
                        if event.detail.type == MatchEventType.OTHER:
                            all_events.append(event)
                        lowest = min(lowest, event.id)

                # skip if no maps were played
                if not all_events: break

                # first pass - find the most common amount of players per map
                playercounts = dict()
                for event in all_events:
                    if not event.detail.type == MatchEventType.OTHER: continue
                    num_players = len(event.game.scores)
                    if num_players not in playercounts:
                        playercounts[num_players] = 0
                    playercounts[num_players] += 1
                most_common_player_count = max(playercounts, key=playercounts.get)

                # second pass - for each map, for each score, write a csv row about that score 
                #     skip any maps with non-osu gamemodes
                #     skip any deleted maps
                #     skip any maps with unusual player counts
                for event in all_events:
                    game = event.game
                    if not game.mode == GameMode.OSU: continue
                    bm = game.beatmap
                    if not bm: continue
                    if not len(game.scores) == most_common_player_count: continue
                    etime = event.timestamp
                    bm_id = game.beatmap_id
                    for score in game.scores:
                        line = (i, 
                                abbr, 
                                etime, 
                                bm_id, 
                                score.user_id, 
                                user_dict[score.user_id], 
                                score.mods, 
                                score.score)
                        await write_csv(line, dest)
            except ValueError:
            # expired mp link
                if (i%500 == 0): 
                    time = datetime.datetime.now()
                    print(f"id {i} process {mod} fail : took {time - prev_time}")
                    prev_time = time
                break
            except TypeError:
                #forbidden mp links
                if (i%30 == 0): print(f"private mp {i}")
                break
            except ConnectionError:
                print(f"connection error {i}")
            except aiohttp.client_exceptions.ServerDisconnectedError:
                print(f"disconnected {i}")
            except aiohttp.client_exceptions.ContentTypeError:
                print(f"content type error {i}")
            except Exception:
                print(f"unknown problem {i}")
                with open("errorlog.txt", "a", newline='', encoding='utf-8') as f:
                    print(f"\n{datetime.datetime.now()} something went wrong id {i} process {mod}", file=f)
                    traceback.print_exc(file=f)
            else:
                break
            finally:
                if attempt >= 9:
                    with open("errorlog.txt", "a", newline='', encoding='utf-8') as f:
                        print(f"\n{datetime.datetime.now()} attempts exhausted {i} process {mod}", file=f)
                    await write_csv((datetime.datetime.now, i), "missing_mps.csv")
    print(f"process {mod} finished")

async def write_csv(obj, filename, mode="a"):
    with open(filename, mode, newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(obj)

def perror(message):
    print(message, file = sys.stderr)

async def main():
    args = sys.argv
    start = 0
    end = 0
    try:
        start = int(args[1])
        end = int(args[2])
    except IndexError:
        if len(args) != 4:               
            perror(f"usage: scrape_scores.py start_mp end_mp [--overwrite]")
            perror(f"error: not enough arguments")
            return
    except ValueError:
        perror(f"usage: scrape_scores.py start_mp end_mp [--overwrite]")
        perror(f"error: invalid int argument: {args[1]} or {args[2]}")
        return
    overwrite = False
    if len(args) == 4: overwrite = args[3] == "--overwrite"
    async with asyncio.TaskGroup() as tg:
        task0 = tg.create_task(scrape_all(0, start, end, overwrite=overwrite))
        task1 = tg.create_task(scrape_all(1, start, end, overwrite=overwrite))
        task2 = tg.create_task(scrape_all(2, start, end, overwrite=overwrite))
        task3 = tg.create_task(scrape_all(3, start, end, overwrite=overwrite))
        task4 = tg.create_task(scrape_all(4, start, end, overwrite=overwrite))
        task5 = tg.create_task(scrape_all(5, start, end, overwrite=overwrite))
        task6 = tg.create_task(scrape_all(6, start, end, overwrite=overwrite))


if __name__ == "__main__":
    asyncio.run(main())            
