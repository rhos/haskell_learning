module Main where

import           Data.Time
import           Database.SQLite.Simple

data Tool = Tool
  { toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
  }

data User = User
  { userId :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [show $ userId user, ".) ", userName user]

instance Show Tool where
  show tool = mconcat
    [ show $ toolId tool
    , ".) "
    , name tool
    , "\n description: "
    , description tool
    , "\n last returned: "
    , show $ lastReturned tool
    , "\n times borrowed: "
    , show $ timesBorrowed tool
    , "\n"
    ]

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

--query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
--query_ :: FromRow r => Connection -> Query -> IO [r]

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

addUser :: String -> IO ()
addUser userName = withConn "layout/tools.db" action
 where
  action conn = do
    execute conn "INSERT INTO users (username) VALUES (?)" $ Only userName
    print "user added"

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "layout/tools.db" action
 where
  action conn = do
    execute conn
            "INSERT INTO checkedout \
        \(user_id,tool_id) VALUES (?,?)"
            (userId, toolId)
    print "checkout added"

printUsers :: IO ()
printUsers = withConn "layout/tools.db" action
 where
  action conn = do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "layout/tools.db" action
 where
  action conn = do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery query
 where
  query = mconcat
    [ "select * from tools "
    , "where id not in "
    , "(select tool_id from checkedout);"
    ]

printCheckedout :: IO ()
printCheckedout = printToolQuery query
 where
  query = mconcat
    [ "select * from tools "
    , "where id in "
    , "(select tool_id from checkedout);"
    ]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <-
    query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing []      = Nothing
firstOrNothing (x : _) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool { lastReturned = date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing     = print "id not found"
updateOrWarn (Just tool) = withConn "layout/tools.db" action
 where
  action conn = do
    let q = mconcat
          [ "UPDATE TOOLS SET "
          , "lastReturned = ?, "
          , "timesBorrowed = ? "
          , "WHERE ID = ?;"
          ]
    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
    print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "layout/tools.db" action
 where
  action conn = do
    tool       <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId = withConn "layout/tools.db" $ \conn ->
  execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- pure read <*> getLine
  print "Enter the id of the tool"
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "enter the id of tool"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

-- performCommand :: String -> IO ()
-- performCommand command
--   | command == "users" = printUsers >> main
--   | command == "tools" = printTools >> main
--   | command == "adduser" = promptAndAddUser >> main
--   | command == "checkout" = promptAndCheckout >> main
--   | command == "checkin" = promptAndCheckin >> main
--   | command == "in" = printAvailable >> main
--   | command == "out" = printCheckedout >> main
--   | command == "quit" = print "bye!"
--   | otherwise = print "Sorry command not found" >> main


main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command

addTool :: String -> String -> IO ()
addTool toolName toolDesc = withConn "layout/tools.db" action
 where
  action conn = do
    execute
      conn
      (mconcat
        [ "INSERT INTO tools "
        , "(name,description,timesBorrowed)"
        , "VALUES (?,?,?)"
        ]
      )
      (toolName, toolDesc, (0 :: Int))
    print "tool added"

promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Enter tool name"
  toolName <- getLine
  print "Enter tool description"
  toolDesc <- getLine
  addTool toolName toolDesc

performCommand :: String -> IO ()
performCommand command | command == "users"    = printUsers >> main
                       | command == "tools"    = printTools >> main
                       | command == "adduser"  = promptAndAddUser >> main
                       | command == "checkout" = promptAndCheckout >> main
                       | command == "checkin"  = promptAndCheckin >> main
                       | command == "in"       = printAvailable >> main
                       | command == "out"      = printCheckedout >> main
                       | command == "quit"     = print "bye!"
                       | command == "addtool"  = promptAndAddTool >> main
                       | otherwise = print "Sorry command not found" >> main
