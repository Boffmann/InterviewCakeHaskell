stock_prices = [10, 7, 5, 8, 11, 9]

get_max_profit :: (Ord a, Num a) => ([a], a, a) -> a
get_max_profit ([], _, max_profit) = max_profit
get_max_profit ([_],_, max_profit) = max_profit
get_max_profit ((f:s:t), curr_min, max_profit)
  | pos_max_profit > max_profit =
        get_max_profit (s:t, new_min, pos_max_profit)
  | otherwise =
        get_max_profit (s:t, new_min, max_profit)
  where   new_min = min f curr_min
          pos_max_profit = s - new_min

main = do
  -- Calculate initial values
  if (length stock_prices) > 1
  then do
    let initial_max_profit = stock_prices !! 1 - stock_prices !! 0
    print $ get_max_profit (tail stock_prices, stock_prices !! 0, initial_max_profit)
  else
    print $ "List to short"
