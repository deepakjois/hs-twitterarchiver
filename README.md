# Introduction

This script will access your Twitter stream and create a text file with all
the past tweets in JSON format. It also supports updating of the text file in
subsequent runs.

The Twitter API currently limits access to a maximum of 3200 tweets in your
timeline. Hence if you have tweeted more often than that, you will not be able
to retrieve beyond 3200 past tweets.

# Installation

You need to have Haskell (GHC 6.10 or 6.12 should do) and Cabal installed.

    cabal install hs-twitterarchiver

# Examples

For help, run `hs-twitterarchiver`

Here is an example of how I use the script to archive all my tweets from 
account `vyom` to a file called `vyom.json`.

    hs-twitterarchiver vyom

If a file called `vyom.json` already exists in the current folder from a previous run,
only the latest Tweets after the previous run will be fetched and the file will 
be updated.

# Feedback

Mail me any feedback you have at deepak.jois@gmail.com
