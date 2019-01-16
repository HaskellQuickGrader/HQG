module CGI.Clone.Clone where
import SystemHook.GroupM
import System.Process

{-This will get called by the system hook when a user is moved to a specific group(determined from config file).  Also in config file
    have a location to clone the repos to on the server.  NOTE: Probably will need some bash scripts to do the cloning part
    
    git log --all --grep=‘search string’-}

--This base URL will come out of the cofing file when it is done    
gitURL :: String -> String
gitURL un = "http://hqd.metatheorem.org/CSCI3300Test/"++un++".git" 


clone :: GroupM -> IO()
clone (GroupM {event_name = en, 
               group_name = gn,
               user_username = uun}) = if en == "user_add_to_group" 
                                       then if gn == "CSCI3300" -- this will eventually be pulled from a config file
                                            then do
                                                let url = gitURL uun
                                                (exitCode,standardOut,standardErr) <- readProcessWithExitCode "CGI/Clone/./CloneRepo.sh" [url,uun] ""
                                                undefined
                                            else return ()
                                       else return()