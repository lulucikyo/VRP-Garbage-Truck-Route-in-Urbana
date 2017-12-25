# VRP-Garbage-Truck-Route-in-Urbana
CEE 512 at UIUC \
Logistics Systems Analysis

This is a project designing vehicle travelling routes in Urbana urban area with constructive heuristic and meta heuristic methods.

## Data Collection and Assumption
The data is collected from Census Bureau. The population, longitude and latitude for each block are incorporated as file UrbanaData.csv. It is assumed each person produces 4.4 lbs of garbage each week on average. The garbage truck will visit each block instead of each house/apartment. 

## Methods
Constructive Heuristic: Sweep \
The sweep algorithm is written in R. It gives 20 tours and in each tour, a MST-based TSP algorithm is used to find the optimal tour length.

Meta Heuristic: Simulated Anealing \
The SA is written in Matlab, which is adapted from https://github.com/lzane/VRP-using-SA-with-Matlab. Since more than 500 blocks (large-scale) are visited, the solution from sweep method is used as the inital solution.

## Scenarios
A: The garbage truck visits each block once a week.\
B: The garbage truck visits blocks with higher demand multiple times a week (time window constraint) \
