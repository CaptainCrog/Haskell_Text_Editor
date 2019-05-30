import Control.Monad()
import Text.Read()
type TextEditor = (String {-LEFT_STRING-}, String{-RIGHT_STRING-}, String{-SELECT_LIST-}, String{-COPY_BUFFER-})

{-TEST COMMANDS
sen = createTextEditor
str = inputString sen "Hello my name is Doppio"
movePrev = moveToPrevWordStart str
movePrevAgain = moveToPrevWordStart movePrev
deleteLetter = delete movePrevAgain
-}


-- FUNCTION DECLARATIONS
create::TextEditor
display::TextEditor->IO()
save::TextEditor->IO()
load::IO()

input::TextEditor->String->TextEditor
copy::TextEditor->TextEditor
paste::TextEditor->TextEditor->TextEditor
delete::TextEditor->TextEditor
backspace::TextEditor->TextEditor


moveLeft::TextEditor->TextEditor
moveRight::TextEditor->TextEditor
moveSenStart::TextEditor->TextEditor
moveSenEnd::TextEditor->TextEditor
moveNextWord::TextEditor->TextEditor
movePrevWord::TextEditor->TextEditor 

selectAllRight::TextEditor->TextEditor
selectAllLeft::TextEditor->TextEditor
selectNextChar::TextEditor->TextEditor
selectPrevChar::TextEditor->TextEditor
selectNextWord::TextEditor->TextEditor
selectPrevWord::TextEditor->TextEditor

-- MISC FUNCTIONS
create = ("","","","")

display(leftString, rightString, _, _) = putStrLn (leftString ++ "|" ++ rightString)

save(leftString, rightString, _, _) = do
    putStrLn "Type file to output text"
    file <- getLine
    writeFile file (leftString++rightString)
    putStrLn "File saved" 

load = do 
    putStrLn "Type file to open"
    file <- getLine
    contents <- readFile file
    let x = input create contents
    print x

copy (_, _, select, copyBuffer) = ("","","",[]++select)

paste (_, _, _, copyBuffer) (leftString, rightString, _, _) = (leftString ++ copyBuffer, rightString, "", "")

delete (leftString, rightString, _, _) = (leftString, drop 1 rightString, "", "")

backspace (leftString, rightString, _, _) = ((init leftString), rightString, "", "")

-- INPUT FUNCTION
input (leftString, rightString, _, _) input = (leftString ++ input, rightString, "", "")

-- MOVE FUNCTIONS	
moveLeft (leftString, rightString, [], _) = ((init leftString), (last leftString):rightString, "","")

moveRight (leftString, rightString, [], _) = (leftString ++ [head rightString], tail rightString, "","")

moveNextWord (leftString, rightString, _, _) = search(moveRight(leftString, rightString,"","")) where 
    search (leftString, rightString, _, _)
     | (([last leftString]) == " " && ([head rightString]) /= " ") = (leftString, rightString, "","")
     | rightString == [] = (leftString, rightString, "","")
     | otherwise = search(moveRight(leftString, rightString, "",""))

movePrevWord (leftString, rightString, _, _) = search(moveLeft(leftString, rightString, "","")) where 
    search(leftString, rightString, _, _)
     | (([last leftString]) == " " && ([head rightString]) /= " ") = (leftString, rightString, "","")
     | leftString == [] = (leftString, rightString, "","")
     | otherwise = search(moveLeft(leftString, rightString, "",""))

moveSenStart (leftString, rightString, _, _)
    | leftString == [] = (leftString, rightString, "","")
    | otherwise = moveSenStart (moveLeft (leftString, rightString, "",""))

moveSenEnd (leftString, rightString, _, _)
    | rightString == [] = (leftString, rightString, "","")
    | otherwise = moveSenEnd (moveRight (leftString, rightString, "",""))

-- SELECT FUNCTIONS
selectNextChar (leftString, rightString, select, _) = (leftString ++ [head rightString], tail rightString, select ++ [head rightString], "")

selectPrevChar (leftString, rightString, select, _) = ((init leftString), (last leftString):rightString, (last leftString):select, "")

selectNextWord (leftString, rightString, select, _) = search(leftString ++ [head rightString], tail rightString, select ++ [head rightString], "") where 
    search(leftString, rightString, select, "")
      | (([last leftString]) == " " && ([head rightString]) /= " ") = (leftString, rightString, select, "")
      | (rightString == []) = (leftString, rightString, select,"")
      | otherwise = search(leftString ++ [head rightString], tail rightString, select ++ [head rightString], "")

selectPrevWord (leftString, rightString, select, _) = search((init leftString), (last leftString):rightString, (last leftString):select, "") where 
    search(leftString, rightString, select, "")
      | (([last leftString]) == " " && ([head rightString]) /= " ") = (leftString, rightString, select, "")
      | (leftString == []) = (leftString, rightString, select,"")
      | otherwise = search((init leftString), (last leftString):rightString, (last leftString):select, "")

selectAllRight (leftString, rightString, select, _) 
    | rightString == [] = (leftString, rightString, select, "")
    | otherwise = selectAllRight (selectNextChar(leftString, rightString, select, ""))

selectAllLeft (leftString, rightString, select, _)
    | leftString == [] = (leftString, rightString, select, "")
    | otherwise = selectAllLeft (selectPrevChar(leftString, rightString, select, ""))
