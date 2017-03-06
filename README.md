# elm-markets

## Examples

```elm

import Market
import Markets

-- Fetch pairs list

type Msg
    = MarketMsg (Result Market.Error Market.Response)

pairs : Cmd msg
pairs =
    Markets.pairs MarketMsg Markets.all

orderBooks : Cmd msg
orderBooks =
    Markets.orderBooks MarketMsg Markets.all [ (Market.Pair Market.BTC Market.USD) ]


recentTrades : Cmd msg
recentTrades =
    Markets.recentTrades MarketMsg Markets.all [ ( Market.Pair Market.BTC Market.USD )]
```
