module Dialog where

import Graphics.UI.Gtk hiding (response)

get_file_name_dialog :: IO ()
get_file_name_dialog = do
    dialog <- fileChooserDialogNew
                (Just $ "Select game state file")
                (Nothing)                                     --the parent window (TODO: why seqfault)
                FileChooserActionOpen                         --the kind of dialog we want
                [("gtk-cancel"                                --The buttons to display
                ,ResponseCancel)
                ,("gtk-open"
                , ResponseAccept)]
    widgetShow dialog
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             putStrLn $ "you selected the file " ++ show fileName
        ResponseCancel -> putStrLn "dialog canceled"
        ResponseDeleteEvent -> putStrLn "dialog closed"
    widgetHide dialog
