module Dialog where

import Graphics.UI.Gtk hiding (response)


get_file_name_dialog_load :: IO (Maybe String)
get_file_name_dialog_load = do
    initGUI
    dialog <- fileChooserDialogNew
                (Just $ "Select game state file")
                (Nothing)
                FileChooserActionOpen
                [("gtk-cancel" ,ResponseCancel) ,("gtk-open" , ResponseAccept)]
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    idleAdd idle_fun 0
    mainGUI
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             putStrLn $ "opened file " ++ show fileName
                             return (Just fileName)
        ResponseCancel -> do putStrLn "dialog canceled"
                             return Nothing
        ResponseDeleteEvent -> do putStrLn "dialog closed"
                                  return Nothing


get_file_name_dialog_save :: IO (Maybe String)
get_file_name_dialog_save = do
    initGUI
    dialog <- fileChooserDialogNew
                (Just $ "Select game state to load")
                (Nothing)
                FileChooserActionSave
                [("gtk-cancel" ,ResponseCancel) ,("gtk-save" , ResponseAccept)]
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    idleAdd idle_fun 0
    mainGUI
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             putStrLn $ "created file " ++ show fileName
                             return (Just fileName)
        ResponseCancel -> do putStrLn "dialog canceled"
                             return Nothing
        ResponseDeleteEvent -> do putStrLn "dialog closed"
                                  return Nothing


idle_fun :: IO Bool
idle_fun = do
    mainQuit
    return False
