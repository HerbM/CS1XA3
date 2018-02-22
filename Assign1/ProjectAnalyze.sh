#Checks if local repo is up to date with the remote repo
if git remote show origin | grep -q "up to date"
then echo "Up to Date"
else echo "Not Up to Date" 
fi

# uncommited changes in a file changes.log. Note, this removes previous content of changes.log 
git diff > changes.log


#Puts each Line from every file containing #TODO in a todo.log
rm todo.log
cat $(grep -r -ln "#TODO" ) | sort -u  > todo.log

#finds  haskell error

if [ "find -name "*.hs" | wc -l" = "0" ] 
then find -name "*.hs" |xargs ghc -fno-code > error.log
else
echo "There are no Haskell programs"
echo "" > error.log
fi

#find -name "*.hs" |xargs ghc -fno-code > error.log

#Asks for git clone
echo "Would you like me to clone a repo? [Y,n]"
read input
if [[ $input == "Y" || $input == "y" ]]; then
        echo "Please enter the repo"
	read input2
	git clone $input2
else
        echo "Okay I wont clone anything"
fi

