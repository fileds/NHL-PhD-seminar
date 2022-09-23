# NHL-PhD-seminar
Data to use at Statistics PhD seminar 23/9, 2022.

## Instructions
### Cloning the repo
Clone the directory by doing a git clone. Otherwise I can also share it on teams, but I figured we'll try GitHub!

### Loading and exploring the data
The data is stored in *dbs/fantasy_df.csv*, but you can use the script *PhD_seminar.R* to explore the data.

### Task
I pose three different problems that you can work on, but you are free to do whatever you like, as long as you gain insights from the data.

Note fantasyPoints are calculated using 

``` R
  mutate(fantasyPoints = case_when(
      position == "G" ~ (2 * wins - 0.8 * goalsAgainst + 0.15 * saves + 3 * shutouts),
      T ~ (2 * assists + 3 * goals + plusMinus * 0.3 + 1 * shortHandedPoints + 0.2 * shots + 0.2 * hits + 0.3 * blocked))
    ) 
```

#### Task 1
Predict fantasyPoints by using historic data. Try to predict next seasons fantasy score based on previous seasons.

#### Task 2
Find a way to compare different positions (G, D, W, C) in a fair way. When is a defensmen worth more than a forward, despite the defenseman scoring less in total?

#### Task 3
Cluster players based on their stats. Try to get players with similar characteristics in groups liek "goalscorers" that only score goals and little else, or "hitters" which deliver a lot of hits. The clustering can then be used for prediction(?).

I will brief you before we start.

## The Data
The data consists of a dataframe with 5653 observations of 50 variables. Each observation is a season that a currently active NHL player *has* played in the NHL. Some of the variables include information like name, position, and team, while most of the variable contain statistics from the sesaons. Some variables only apply to goalkeepers, see the *PhD_seminar.R* script. 

Below is a description of the variables containing information.

| Variable | Meaning |
| -------- | ------- |
| id       | PLayer identification |
| season   | The year the season started |
| fullName | The players name, may not be unique. Use *id* |
| position | The players position. May take values "G", "D", "W", "C" |
| team     | Players team that they finished the season with. |
| league   | Redundant, always NHL in the fantasy DB |
