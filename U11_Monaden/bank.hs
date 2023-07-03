type Money = Int
type Balance = Money
type Account = (Money, Money) -- Debit and Credit

-- so ziemlich die dümmste Implementierung von Bankaccounts, die ich je machen musste
-- warum hebt man einen negativen Betrag ab??
withdraw :: Money -> Account -> Maybe Account
withdraw amount (debit, credit)
    | -1 * (debit + amount) >= credit = Nothing -- Kontoüberziehung, Konto wird gesperrt
    | otherwise = Just (debit + amount, credit)

deposit :: Money -> Account -> Maybe Account
deposit amount (debit, credit)
    | -1 * debit >= credit + amount = Nothing
    | otherwise = Just (debit, credit + amount)

example1 :: Maybe Account
example1 = do
    account1 <- deposit 90000000 (0, 0)
    account2 <- withdraw (-40000000) account1
    account3 <- withdraw (-10000000) account2
    account4 <- withdraw (-45000000) account3
    deposit 6000000 account4

example2 :: Maybe Account
example2 = do
    acc1 <- deposit 20000000 (0, 0)
    acc2 <- deposit 40000000 acc1
    acc3 <- withdraw (-15000000) acc2
    acc4 <- withdraw (-25000000) acc3
    deposit 10000000 acc4

example1bind :: Maybe Account
example1bind =
    deposit 90000000 (0, 0)
        >>= withdraw (-40000000)
        >>= withdraw (-10000000)
        >>= withdraw (-45000000)
        >>= deposit 6000000

example2bind :: Maybe Account
example2bind =
    deposit 20000000 (0, 0)
        >>= deposit 40000000
        >>= withdraw (-15000000)
        >>= withdraw (-25000000)
        >>= deposit 10000000

result1 :: Maybe Balance
result1 = example1bind >>= accountState
result2 :: Maybe Balance
result2 = example2bind >>= accountState

accountState :: Account -> Maybe Balance
accountState (debit, credit)
    | credit + debit < 0 = Nothing
    | otherwise = Just (credit + debit)