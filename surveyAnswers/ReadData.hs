{-# LANGUAGE DeriveDataTypeable #-}
module ReadData where
import Text.JSON.Generic

data Question = Question
    { text              :: String
    , survey_answers    :: [SurveyAnswer]
    } deriving (Show, Data, Typeable)


data SurveyAnswer = SurveyAnswer
    { respondent_id  :: Integer
    , score          :: Integer
    } deriving (Show, Data, Typeable)

data Address = Address
    { house  :: Integer
    , street :: String
    , city   :: String
    , state  :: String
    , zip    :: Integer
    } deriving (Show, Data, Typeable)

data Person = Person
    { name    :: String
    , age     :: Integer
    , address :: Address
    } deriving (Show, Data, Typeable)

aa :: String
aa = "{\"name\": \"some body\", \"age\" : 23, \"address\" : {\"house\" : 285, \"street\" : \"7th Ave.\", \"city\" : \"New York\", \"state\" : \"New York\", \"zip\" : 10001}}"
main = print (decodeJSON aa :: Person)

importFile' = 
    do 
        file <- readFile "data.json"
        decode' file

--printfstline :: String -> [Question]
--printfstline file = print (decodeJSON $ head (splitOn "\n" file) :: [Question])
decode' file = print (decodeJSON file :: [Question])