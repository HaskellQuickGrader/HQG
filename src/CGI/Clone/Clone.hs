module CGI.Clone.Clone where
import SystemHook.GroupM

{-This will get called by the system hook when a user is moved to a specific group(determined from config file).  Also in config file
    have a location to clone the repos to on the server.  NOTE: Probably will need some bash scripts to do the cloning part
    
    git log --all --grep=‘search string’-}
clone :: GroupM -> IO()
clone (GroupM {event_name = en, 
               group_name = gn,
               user_username = uun}) = if en == "user_add_to_group" 
                                       then if gn == "CSCI3300" -- this will eventually be pulled from a config file
                                            then undefined
                                            else return ()
                                       else return()