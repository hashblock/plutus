{-# LANGUAGE OverloadedStrings #-}
module Option where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

contract :: Contract
contract = When
    [Case
        (Deposit
            (mkRole "party")
            (mkRole "party")
            (Token (currencySymbol "") (tokenName "ada"))
            (Constant 1000)
        )
        (When
            [Case
                (Deposit
                    (mkRole "counterparty")
                    (mkRole "counterparty")
                    (Token (currencySymbol "") (tokenName "bitcoin"))
                    (Constant 1)
                )
                (When
                    [Case
                        (Choice
                            (ChoiceId
                                (fromHaskellByteString "exercise")
                                (mkRole "party")
                            )
                            [Bound 0 1]
                        )
                        (If
                            (ValueEQ
                                (ChoiceValue
                                    (ChoiceId
                                        (fromHaskellByteString "exercise")
                                        (mkRole "party")
                                    ))
                                (Constant 1)
                            )
                            (Pay
                                (mkRole "counterparty")
                                (Party (mkRole "party"))
                                (Token (currencySymbol "") (tokenName "bitcoin"))
                                (Constant 1)
                                (Pay
                                    (mkRole "party")
                                    (Party (mkRole "counterparty"))
                                    (Token (currencySymbol "") (tokenName "ada"))
                                    (Constant 1000)
                                    Close
                                )
                            )
                            (Pay
                                (mkRole "party")
                                (Party (mkRole "counterparty"))
                                (Token (currencySymbol "") (tokenName "ada"))
                                (Constant 100)
                                Close
                            )
                        )]
                    1000 Close
                )]
            10 Close
        )]
    10 Close
