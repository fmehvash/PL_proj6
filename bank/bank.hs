module Bank where

import Control.Monad.State

-- Define the state structure for the bank
data BankState = BankState
    { balance :: Float
    , overdrawn :: Bool
    } deriving Show

-- Define the BankOp type as a type synonym for State monad
type BankOp = State BankState

-- Function to run a BankOp starting with the initial state
runBankOp :: BankOp a -> a
runBankOp op = evalState op initialState
  where
    initialState = BankState 0 False

-- Deposit function: adds the specified amount to the balance
deposit :: Float -> BankOp ()
deposit amount = modify (\s -> s { balance = balance s + amount })

-- Withdraw function: subtracts the specified amount and enforces overdraft limits
withdraw :: Float -> BankOp Float
withdraw amount = do
    currBalance <- gets balance
    let actualWithdraw = min (currBalance + 100) amount  -- Allow overdraft up to $100
    modify (\s -> s { balance = currBalance - actualWithdraw
                    , overdrawn = (currBalance - actualWithdraw) < 0 })
    return actualWithdraw

-- Get balance function: retrieves the current balance
getBalance :: BankOp Float
getBalance = gets balance