#hecks if local repo is up to date with the remote repo
if git remote show origin | grep -q "up to date"
then echo "Up to Date"
else echo "Not Up to Date" 
fi

# uncommited changes in a file changes.log. Note, this removes previous content of changes.log 
git diff > changes.log


#Puts each Line from every file containing #TODO in a todo.log
rm todo.log
cat $(grep -r -ln "#TODO" ) | sort -u  > todo.log

#findsi haskell error
if [ "find -name "*.hs" | wc -l" = "0" ]
then find -name "*.hs" |xargs ghc -fno-code > error.log
else
echo "There are no Haskell programs"
echo "" > error.log
fi

#Asks for git clone
echo "Would you like me to clone a repo? [Y,n]"
read input
if [[ $input == "Y" || $input == "y" ]]; then
        echo "Please enter the repo"
	read input2
	git clone $input2
else
        echo "Okay I won't clone anything"
fi

if !  git remote show origin | grep -q "up to date" 
then echo "As you are not up to date, would you like me to perform a git pull?"
read answer
if [[ $answer == "Y" || $answer == "y" ]]
then git pull
else 
echo "Alright I will not perform a git pull."
fi
fi

echo "Would you like to hear a joke"
read whatever
echo "Okay, Deleted all files modified in the last week."

