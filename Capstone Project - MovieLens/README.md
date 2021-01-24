# Capstone Project - Recommender System

## Introduction

This project was offered as part of the edX - HarvardX Data Science course. The main purpose of this project was to create a movie recommendation system using a small subset of a much larger MovieLens dataset.
Recommender systems are a specific class of machine learning techniques that are widely used in web ap- plications like e-commerce sites, news sites, social networks or streaming services. The main purpose of recommenders is to personalize user experience via suggesting to the end-user (consumer) something that will enhance their overall experience or help to make a decision. These recommendations are based on pre- vious user experience call collaborative filtering. For example, two users who shared similar interests in the past might have a similar interest in the future. Therefore we can offer them certain items that will match their interests. Collaborative recommenders take into account only user preferences and ignore the charac- teristics or content of items they recommend. The other type of recommenders is content-based systems. These systems take into consideration similarities between items and similarities between user data and require a large amount of data for better accuracy. There are also, knowledge-based systems that take into consideration information about the items/products and match them with user historical data. And finally, there are hybrid systems that combine features of previously mentioned systems into one recommender, this approach can reduce the disadvantages of both systems and create a more robust recommender.

## Project Goal

The project goal was to create a machine learning model that can predict movie ratings using data provided in edX data set. To evaluate the model the validation data set was provided as well. Root Mean Square Error was used as a loss function to measure the accuracy of the models (finding the difference (error) between ground truth rankings and predicted rankings).Low RMSE means that the model preforms well and predicts rankings accurately.

## Data

In this project the MovieLens 10M data set was used. The data set was created by GroupLens research that has collected and made it available. The data set has 10 million ratings and 100,000 tag applications applied to 10,000 movies by 72,000 users.

### Please check PDF report for the detailed project overview and data exploration and resutls
