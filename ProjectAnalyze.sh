#Checks if local repo is up to date with the remote repo
git remote show origin


# uncommited changes in a file changes.log. Note, this removes previous content of changes.log 
git diff < changes.log

# pastChanges contains all 
git diff << pastChanges.log

#Puts each Line from every file containing #TODO in a todo.log
cat $(grep -l -r "#TODO" ) > todo.log

