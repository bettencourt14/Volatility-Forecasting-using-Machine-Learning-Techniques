# Volatility-Forecasting-using-Machine-Learning-Techniques
This is the database for my personal Master's degree dissertation, in which I pretend to compare the forecasting capacity of multiple models, when it comes to forecasting the stock index volatility after 2 weeks of war in Ukraine.
the models to be use under this analysis are the following:

- Monte Carlo Simulation, following a Geometric Bronwian Motion
- GARCH (1,1)
- GARCH-MIDAS, including low frequency variables
- Support Vector Regression, including high freqeuncy variables
- LSTM
- Transformer

Once the paper is submited and available on a public repository i will added it here.

Methodology Process:

- Please always check the Source and directory before proceeding

- do a primary analysis on the data for the 4 sets
it will be based on this data sets that the models will work.

-Run Monte Carlo for each Index
-Run Garch for each Index
-Run Garch-MIDAS for each Index (use low frequency data for the COuntry/Set of Countries)
-Run SVR for each Index (use High frequency data)
-Run LSTM for each Index 
-Run Transformer for each Index

-Pick all R Code in put in a big Script, add the tables from Python Code to R Code

- Create a table by Index, of ABS, MSE, RMSE and by model, to check which model performed better

- Use a Diebold-Mariano to check if the Accuracy measurements are correct.


