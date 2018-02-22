#Checks if local repo is up to date with the remote repo
if git remote show origin | grep -q "up to date"
then echo "Up to Date"
else echo "Not Up to Date" 
fi

# uncommited changes in a file changes.log. Note, this removes previous content of changes.log 
git diff > changes.log


#Puts each Line from every file containing #TODO in a todo.log
rm todo.log
echo $(grep -ln "#TODO" ) | sort -u  > todo.log


#finds haskell errors
find -name "*.hs" | xargs ghc -fno-code > error.log
