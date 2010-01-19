# Introduction

This script will access your Twitter stream and create a text file with all 
the past tweets in JSON format.

The Twitter API currently limits access to a maximum of 3200 tweets in your
timeline. Hence if you have tweeted more often than that, you will not be able
to retrieve beyond 3200 past tweets.

# Installation

You need to have Haskell (GHC 6.10 or 6.12 should do) and Cabal installed.

    cabal install hs-twitterarchiver

# Examples

For all options, run `hs-twitterarchiver -h`

Here is an example of how I use the script to archive all my tweets from 
account `vyom` to a file called `vyom.json`. This is also the default behavior
when `hs-twitterarchiver` is called without any arguments.

    runhaskell twitterarchiver.hs -u vyom -f vyom.json

If you have a private stream, you can call the script with a `-p` argument and
you will be prompted for a password. The script will then automatically use
HTTP Basic authentication when calling the Twitter API.

    runhaskell twitterarchiver.hs -u divya -f divya.json -p
    Enter Twitter Password :

# Feedback

Mail me any feedback you have at deepak.jois@gmail.com
