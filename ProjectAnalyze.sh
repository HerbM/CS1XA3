#Checks if local repo is up to date with the remote repo
if git remote show origin | grep -q "up to date"
then echo "Up to Date"
else echo "Not Up to Date" 
fi

# uncommited changes in a file changes.log. Note, this removes previous content of changes.log 
git diff > changes.log


#Puts each Line from every file containing #TODO in a todo.log
cat $(grep -l -r "#TODO" ) > todo.log

