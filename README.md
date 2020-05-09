# Twitter Stock Market Sentiment Analysis

### Preliminary work:
- Dataset: My choice of 6 stocks, 3 largest gainer and 3 loser stocks for the day (5/1/2020) (Source: [Yahoo Finance](https://finance.yahoo.com/gainers))
- Use the R Twitter API to perform stock market sentiment analysis. (Developer tool: [Twitter API](https://developer.twitter.com/en))
- Before performing analysis, you need to apply for a Twitter developer account filling out an application for API access.
- Create an "App" in developer account. Remember to get your Consumer API keys and tokens.

### Main work:
- Make necessary addition (if needed) to the lexicons (the positive and negative word lists), and comupute the sentiment score of all
the tweets for each gainers (losers) set.
- Plot a bar chart of the sentiment. Use ***googleVis*** R package to create a candlestick plot of
the stock prices for the stocks used for this project and compare them with a chart or table obtained 
from an online financial source (downloaded .csv file from Yahoo Finance).

### Process steps:
1. Search for 100 tweets for each top gainers and losers (like $AVTR)
2. Combine 3 top gainers / losers into a whole tweet and create corpus.
3. Preprocessing: remove whitespace & numberwords etc. in content
4. Make Term Document Matrix for these 6 stocks, and compute each frequent terms then display as WordCloud.
5. Compute sentiment score based on “positive-words.txt” and “negative-words.txt”.
Plot the bar chart of sentiment score, and use “googleVis” package to draw candlestick chart of the stock.

### My conclusion:
- Consequently, I found that gainers has positive sentiment score (score=8), 
while losers has negative score (score=-6), which means the tweets contents related to those 
gainers has more positive words thereby people may be optimistic about them. In contrast, losers stocks has been proved that they gained more unpromising or negative views by people.
- In short, according to the result of sentiment score and detailed analysis given above, we conclude that the outcomes of sentiment analysis in Twitter stock market conform to the expected perspective that gainers stocks reveal positive score while losers stocks reveal negative score.

