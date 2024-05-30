import constants
import asyncio
import time
import datetime
import sys
import csv
import traceback
import aiohttp
from time import sleep
from constants import *
from ossapi import OssapiAsync, UserLookupKey, GameMode, RankingType, Ossapi, User

api = OssapiAsync(CLIENT_ID, API_KEY)

async def gather_players(mod, filename):
    i = 0
    dest = f"csvfiles/user_data/user_data_{mod}.csv"
    overwrite = True
    if overwrite: 
        line = ("UserID", 
                        "Username",
                        "GlobalRank", 
                        "PP", 
                        "Location",
                        "RegistrationDate")
        await write_csv(line, dest, mode="w")
    prev_time = datetime.datetime.now()
    #"csvfiles/user_data/all_player_ids.csv"
    with open(filename, "r", newline='', encoding='utf-8') as f:
        reader = csv.reader(f)
        for row in reader:
            i += 1
            if not (i%7 == mod):
                continue
            for attempt in range(10):
                try:
                    id = int(row[0])
                    user_obj = await api.user(id, mode=GameMode.OSU, key=UserLookupKey.ID)
                    user_stats = user_obj.statistics
                    line = (id, 
                                user_obj.username, 
                                user_stats.global_rank, 
                                user_stats.pp,
                                user_obj.country.name,
                                user_obj.join_date)
                    await write_csv(line, dest)
                    if (i%200 == 0):
                        times = datetime.datetime.now()
                        print(f"process {mod} id {i} : took {times - prev_time}")
                        prev_time = times
                except ValueError:
                    print(f"User {row[0]} not found")
                    break
                except Exception as e:
                    print(f"Error {e}")
                    time.sleep(0.2)
                else:
                    break
    print(f"process {mod} finished")

async def write_csv(obj, filename, mode="a"):
    with open(filename, mode, newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(obj)

def perror(message):
    print(message, file = sys.stderr)

async def main():
    args = sys.argv
    try:
        ids_file = args[1]
        with open(ids_file, "r", newline='', encoding='utf-8') as f:
            pass
    except IndexError:
        perror(f"usage: scrape_users.py filepath")
        perror(f"error: no user id file provided")
        return
    except FileNotFoundError:
        perror(f"error: no such file or directory: '{ids_file}'")
        return
    async with asyncio.TaskGroup() as tg:
        task0 = tg.create_task(gather_players(0, ids_file))
        task1 = tg.create_task(gather_players(1, ids_file))
        task2 = tg.create_task(gather_players(2, ids_file))
        task3 = tg.create_task(gather_players(3, ids_file))
        task4 = tg.create_task(gather_players(4, ids_file))
        task5 = tg.create_task(gather_players(5, ids_file))
        task6 = tg.create_task(gather_players(6, ids_file))

if __name__ == "__main__":
    asyncio.run(main())            
