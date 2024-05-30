# osu! RME Tournament Rating System
## INFO 201 "Foundational Skills for Data Science"

Authors: Anh-Minh Nguyen (anhminh.nguyen314 (at) gmail.com)

**Link: https://a-nguyen.shinyapps.io/final-project-anguyenuw/**

# Introduction
Players of the online rhythm game osu! often question if the official ranking system accurately rates player skill. To explore this, we calculated a Rhythm Metrics Elo (RME) for every osu! tournament participant from Jan 1 2023 to May 20 2024.

## Questions
- Who are the best tournament players?
- Are highly ranked players on the official leaderboards similarly highly ranked in tournament performance?
- Are highly performant players in tournaments similarly highly ranked in the official leaderboards?
- Does geographic location have any effect on a player's skill?
- Is the official osu! ranking system representative of overall skill?

# Conclusion / Summary Takeaways

In calculating RME ratings for all tournament participants, we found a strong negative logarithmic correlation between official rank and RME rating - as a player’s rank decreases (i.e. gets closer to #1), their RME rating increases. We also found that most top players in one system are also near the top in the other. Because of this, we believe that the official osu! ranking system is a good indicator of overall skill. However, it isn’t perfect, since we can find low-ranked players (below rank 10,000) who play at a very high level in tournaments. This implies there is more work to be done in accurately rating a player’s skill in osu!.