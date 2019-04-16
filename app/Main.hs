module Lib where
import Lib
import System.IO()
import Control.Monad()
import Read.Text
type TextEditor = (String {-LEFT_STRING-}, String{-RIGHT_STRING-}, String{-SELECT_LIST-}, String{-COPY_BUFFER-}, String{-PASTE_LIST-})



-- FUNCTION DECLARATIONS
{-NOTE: Functions main, saveFile, and loadFile don't need a function declaration because of import System.IO() at the top of the document-}
createTextEditor::TextEditor
setCursor::TextEditor->IO()

inputString::TextEditor->String->TextEditor

moveLeft::TextEditor->TextEditor
moveRight::TextEditor->TextEditor
moveToSenStart::TextEditor->TextEditor
moveToSenEnd::TextEditor->TextEditor
moveToNextWordStart::TextEditor->TextEditor
moveToNextWordEnd::TextEditor->TextEditor
moveToPrevWordStart::TextEditor->TextEditor 
moveToPrevWordEnd::TextEditor->TextEditor

selectAllRight::TextEditor->TextEditor
selectAllLeft::TextEditor->TextEditor
selectNextChar::TextEditor->TextEditor
selectPrevChar::TextEditor->TextEditor
selectNextWord::TextEditor->TextEditor
selectPrevWord::TextEditor->TextEditor

-- MISC FUNCTIONS
main = do
    contents <- readFile "test.txt"
    putStrLn contents

createTextEditor = ("","","","","")

setCursor(leftString, rightString, _, _, _) = putStrLn (leftString ++ "|" ++ rightString)

saveFile = do
    putStrLn "Type file to output text"
    file <- getLine
    writeFile file (show createTextEditor)
    putStrLn "File saved" 

loadFile = do 
    putStrLn "Type file to open"
    file <- getLine
    let errCheck =  readMaybe 
    contents <- readFile file
    putStrLn contents

-- INPUT FUNCTION
inputString (leftString, rightString, _, _, _) input = (leftString ++ input, rightString, "", "", "")

-- MOVE FUNCTIONS	
moveLeft (leftString, rightString, _, _, _) = ((init leftString), (last leftString):rightString, "","","")

moveRight (leftString, rightString, _, _, _) = (leftString ++ [head rightString], tail rightString, "","","")

moveToSenStart (leftString, rightString, _, _, _)
    | leftString == [] = (leftString, rightString, "","","")
    | otherwise = moveToSenStart (moveLeft (leftString, rightString, "","",""))

moveToSenEnd (leftString, rightString, _, _, _)
    | rightString == [] = (leftString, rightString, "","","")
    | otherwise = moveToSenEnd (moveRight (leftString, rightString, "","",""))

moveToNextWordStart (leftString, rightString, _, _, _) = searchNextWordStart(moveRight(leftString, rightString,"","","")) where 
     searchNextWordStart (leftString, rightString, _, _, _)
      | (([last leftString]) == " " && ([head rightString]) /= " ") = (leftString, rightString, "","","")
      | otherwise = searchNextWordStart(moveRight(leftString, rightString, "","",""))

moveToNextWordEnd (leftString, rightString, _, _, _) = searchNextWordEnd(moveRight(leftString, rightString, "","","")) where 
     searchNextWordEnd (leftString, rightString, _, _, _)
      | (([last leftString]) /= " " && ([head rightString]) == " ") = (leftString, rightString, "","","")
      | otherwise = searchNextWordEnd(moveRight(leftString, rightString, "","",""))

moveToPrevWordStart (leftString, rightString, _, _, _) = searchPrevWordStart(moveLeft(leftString, rightString, "","","")) where 
     searchPrevWordStart (leftString, rightString, _, _, _)
      | (([last leftString]) == " " && ([head rightString]) /= " ") = (leftString, rightString, "","","")
      | otherwise = searchPrevWordStart(moveLeft(leftString, rightString, "","",""))

moveToPrevWordEnd (leftString, rightString, _, _, _) = searchPrevWordEnd(moveLeft(leftString, rightString, "","","")) where 
     searchPrevWordEnd (leftString, rightString, _, _, _)
      | (([last leftString]) /= " " && ([head rightString]) == " ") = (leftString, rightString, "","","")
      | otherwise = searchPrevWordEnd (moveLeft(leftString, rightString, "", "", ""))

-- SELECT FUNCTIONS
selectAllRight (_, rightString, _, _, _) = ("", "", rightString, "", "")

selectAllLeft (leftString, _, _, _, _) = ("", "", leftString, "", "")

selectNextChar (leftString, rightString, select, _, _) = (leftString ++ [head rightString], tail rightString, select ++ [head rightString], "", "")

selectPrevChar (leftString, rightString, select, _, _) = ((init leftString), (last leftString):rightString, (last leftString):select, "", "")

selectNextWord (leftString, rightString, select, _, _) = searchNextWordStart(selectNextChar(leftString, rightString, select, "", "")) where 
     searchNextWordStart(leftString, rightString, select, "", "")
      | (([last leftString]) == " " && ([head rightString]) /= " ") = ("", "", select, "", "")
      | (rightString == []) = ("","",select,"", "")
      | otherwise = searchNextWordStart(selectNextChar(leftString, rightString, select, "", ""))

selectPrevWord (leftString, rightString, select, _, _) = searchPrevWordStart(selectPrevChar(leftString, rightString, select, "", "")) where 
     searchPrevWordStart(leftString, rightString, select, "", "")
      | (([last leftString]) == " " && ([head rightString]) /= " ") = ("", "", select, "", "")
      | (leftString == []) = ("","",select,"", "")
      | otherwise = searchPrevWordStart(selectPrevChar(leftString, rightString, select, "", ""))