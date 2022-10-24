import random
import math
import numpy as np
import pandas as pd
from datetime import datetime
from zoneinfo import ZoneInfo
import collections
import numba

today = datetime.now(tz=ZoneInfo("US/Eastern"))
name = today.strftime("%Y-%m-%d")
outname = today.strftime("%Y-%m-%dT%H_%M_%S-04_00")

f = pd.read_csv(f"~/R Stuff/PoliStat/Class Model/For Simulation/{name}.csv")
state = f["state"].to_list()
office = f["office"].to_list()
bpi = pd.to_numeric(f["BPI"]).to_numpy()
polls = pd.to_numeric(f["polls"]).fillna(f["BPI"] / 100).to_numpy() * 100  # n/a -> no polls
variance = pd.to_numeric(f["variance"]).to_numpy()
stdev = pd.to_numeric(f["standard_deviation"]).to_numpy()
numpolls = pd.to_numeric(f["number_of_polls"]).to_numpy()
numpolls30 = pd.to_numeric(f["last_30_polls"]).to_numpy()

us_state_to_abbrev = {
    "Alabama": "AL",
    "Alaska": "AK",
    "Arizona": "AZ",
    "Arkansas": "AR",
    "California": "CA",
    "Colorado": "CO",
    "Connecticut": "CT",
    "Delaware": "DE",
    "Florida": "FL",
    "Georgia": "GA",
    "Hawaii": "HI",
    "Idaho": "ID",
    "Illinois": "IL",
    "Indiana": "IN",
    "Iowa": "IA",
    "Kansas": "KS",
    "Kentucky": "KY",
    "Louisiana": "LA",
    "Maine": "ME",
    "Maryland": "MD",
    "Massachusetts": "MA",
    "Michigan": "MI",
    "Minnesota": "MN",
    "Mississippi": "MS",
    "Missouri": "MO",
    "Montana": "MT",
    "Nebraska": "NE",
    "Nevada": "NV",
    "New Hampshire": "NH",
    "New Jersey": "NJ",
    "New Mexico": "NM",
    "New York": "NY",
    "North Carolina": "NC",
    "North Dakota": "ND",
    "Ohio": "OH",
    "Oklahoma": "OK",
    "Oregon": "OR",
    "Pennsylvania": "PA",
    "Rhode Island": "RI",
    "South Carolina": "SC",
    "South Dakota": "SD",
    "Tennessee": "TN",
    "Texas": "TX",
    "Utah": "UT",
    "Vermont": "VT",
    "Virginia": "VA",
    "Washington": "WA",
    "West Virginia": "WV",
    "Wisconsin": "WI",
    "Wyoming": "WY",
    "District of Columbia": "DC",
    "American Samoa": "AS",
    "Guam": "GU",
    "Northern Mariana Islands": "MP",
    "Puerto Rico": "PR",
    "United States Minor Outlying Islands": "UM",
    "U.S. Virgin Islands": "VI",
}
state = [us_state_to_abbrev[i] for i in state]
for i in range(0, len(office)):
    if office[i] == "Senate 1":
        office[i] = "Senate"
    if office[i] == "Senate 2":
        office[i] = "Senate"
        state[i] = "OK2"

# Correlation here
#senate_seats = [36,29] #[dem, rep], number of sen seats not up for reelection
#governorships = [6,8] #[dem, rep], number of gov seats not up for reelection

sen_races = ['Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maryland', 'Missouri', 'Nevada', 'New Hampshire', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma_1', 'Oklahoma_2', 'Oregon', 'Pennsylvania', 'South Carolina', 'South Dakota', 'Utah', 'Vermont', 'Washington', 'Wisconsin']
gov_races = ['Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Iowa', 'Kansas', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Nebraska', 'Nevada', 'New Hampshire', 'New Mexico', 'New York', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Vermont', 'Wisconsin', 'Wyoming']
all = []
all_races = []
all = sen_races + gov_races
all.sort()
all_races = all
    
# Run simulation
matrix = [0 for i in range(71)] # matrix with 71 rows with each row being a race 
runCount = 1_000_000
counter = 0

df = pd.read_csv("demographics z-scores.csv")  # pulling race demographics from .csv data file
correlation_matrix = np.zeros((71, 71))  # zeros used only to create matrix


for j in range(len(correlation_matrix)): 
    for i in range(71):
        #print(all_races[j], all_races[i])
        if all_races[j] == all_races[i]:
            correlation_matrix[j, i] = 0.6
        else:
            race1 = df[all_races[j]]
            #print(race1)
            race2 = df[all_races[i]]
            #print(np.corrcoef(race1, race2))
            corrcoefs = np.corrcoef(race1, race2)
            #print(corrcoefs[1, 0])
            correlation_matrix[j, i] = corrcoefs[1, 0]
    

for j in range(len(correlation_matrix)): #prevent races from influencing their own results
    for i in range(71):
        if j==i:
            correlation_matrix[j,i] = 0

correlation_matrix[25,26] = 0.8  # setting the two OK races to same-state correlation value; review w/ cc
correlation_matrix[26,25] = 0.8

# SOLUTION 1
# correlation_matrix += 1
# correlation_matrix /= 2

a = 0
with open(f"~/R Stuff/PoliStat/Class Model/For Simulation/National_Mood/{name}_National_Mood.csv", "r") as f:
    for i in f:
        if a == 0:
            pass
        else:
            read = i.split(",")
            mood = 100 * float(read[0]) - 50
            mood_stdev = float(read[2])
        a += 1

@numba.njit
def simulate(bpi, numpolls30, numpolls, polls, stdev, matrix, office, runCount):
    demseats = []
    for _ in range(runCount):
        natmood = random.normalvariate(mood, mood_stdev)
        baboon = np.zeros(71)
        for i in range(0, len(bpi)):
            if office[i] == "Senate":
                baboon[i] = bpi[i] + 0.15 * natmood
            else:
                baboon[i] = bpi[i]

        lean = np.zeros(71)
        for i in range(0, 71):
            weight = (1.9 / math.pi) * math.atan((1.75 * numpolls30[i] + 0.05 * numpolls[i]))
            counterweight = 1 - weight
            combined = weight * polls[i] + counterweight * baboon[i]  
            lean[i] = combined

        variance_differences = np.random.standard_normal(71)
        for i in range(len(stdev)):
            variance_differences[i] *= stdev[i]

        seatsWon = 0

        for i in range(0, 71):
            correlation_effect = np.dot(variance_differences, correlation_matrix[i])/sum(correlation_matrix[i])  # dot product of differences and coefs divided by sum of coefs

            # SOLUTION 2
            correlation_effect = max(-5, min(5, correlation_effect))

            # if state[i] == "MO": print("MO", correlation_effect)
            # if state[i] == "NY": print("NY", correlation_effect)
            
            correlation = lean[i] + variance_differences[i] + correlation_effect
            randomPoll = random.normalvariate(correlation, stdev[i])
            if randomPoll > 50:
                matrix[i] += 1
                if office[i] == "Senate":
                    seatsWon += 1

        demseats.append(36 + seatsWon)

    lean = np.zeros(71)
    for i in range(0, 71):
        weight = (1.9 / math.pi) * math.atan((1.75 * numpolls30[i] + 0.05 * numpolls[i]))
        counterweight = 1 - weight
        combined = weight * polls[i] + counterweight * bpi[i]
        lean[i] = combined
        
    return lean, demseats

# ?? why do we use this? shouldn't lean be calculated as if natmood is 0?
lean, demseats = simulate(bpi, numpolls30, numpolls, polls, stdev, matrix, office, runCount)
demseats = collections.Counter(demseats)

with open('~/Documents/polistat-results-2022/averaged-polls/' + outname + ".csv", "w") as g:
    g.write("state_po,office,BPI,weighted_polls,weighted_sd,weighted_var,lean,dem_wins\n")
    for i in range(0, len(matrix)):
        g.write(state[i] + "," + office[i] + "," + str(bpi[i]) + "," + str(polls[i]) + "," + str(stdev[i]) + "," + str(variance[i]) + "," + str(lean[i]) + "," + str(100 * matrix[i] / runCount))
        if i < len(matrix) - 1:
            g.write("\n")

with open('~/Documents/polistat-results-2022/overall-senate/' + outname + ".csv", "w") as g:
    g.write("demSeats,occurrences\n")
    for i in range(0, 101):
        g.write(str(i) + "," + str(demseats[i]))
        if i < 101 - 1:
            g.write("\n")
        

print("done")
