#1
if git remote show origin | grep -q "up to date"
then echo "Up to Date"
else echo "Not Up to Date" 
fi

#2
git diff > changes.log


#3
(grep -r --exclude="todo.log" --exclude="ProjectAnalyze.sh" --exclude="changes.log"  "#TODO" $pwd ) > todo.log

#4
if [ "find -name "*.hs" | wc -l" = "0" ]
then find -name "*.hs" |xargs ghc -fno-code >> error.log
else
echo "There are no Haskell programs"
echo "" >> error.log
fi

#5
if [ "find -name "*.py" | wc -l" = "0" ]
        then find -name "*.py" |xargs python -m py_compile >> PythonError.log
        else
        echo "There are no Python programs"
        echo "" >> PythonError.log
fi


#6
echo "Would you like me to clone a repo? [Y,n]"
read input
if [[ $input == "Y" || $input == "y" ]]; then
        echo "Please enter the repo"
	read input2
	git clone $input2
else
        echo "Okay I won't clone anything"
fi

#7
if !  git remote show origin | grep -q "up to date" 
then echo "As you are not up to date, would you like me to perform a git push? [Y,n]"
read answer
if [[ $answer == "Y" || $answer == "y" ]]
then git push
else 
echo "Alright I will not perform a git push. [Y,n]"
fi
fi

#8
echo "Would you like to know how much space you have used in your account? [Y,n]"
read query
if [[ $query == "Y" || $query == "y" ]]
then echo "The amount of the memory you used up is:"
du -hcs ~ | tail -n1 | sed 's/\<total\>//g' | sed 's/\<M\>//g'
fi


#9 
echo "would you like me to search for anything?[Y,n]"
read feedback
if [[ $feedback == "Y" || $feedback == "y" ]]
        then echo "Would you like to me to search inside files or filenames? (1 = Inside, 2 = Filename)"
        read feedback2
	echo "What would you like me to search?"
	read searchiteam
        echo "Where would you like me to search? (1 = Current Directory, 2 = Root Directory, 3 = Specific Directory)"
                read feedback3
                if [[ $feedback3 == "3" ]]
                        then echo "Enter Specific Directory"
                        read directory
                        fi
fi

if [ "$feedback2" == "1" ] #'$feedback2' == "1" ]
	then #grep
	if [ "$feedback3" == "1" ] #current directory
	then echo "Here are the files that contain $searchiteam found in the current directory."
	grep -r -l $searchiteam $pwd
	fi

	if [ "$feedback3" == "2" ] #Root Directory
	then echo "Here are the files that contain $searchiteam found in the root directory"
	grep -r -l $searchiteam ~
	fi
	
	if [ "$feedback3" == "3" ] #requested directory
	then echo "Here are the files that contain $searchiteam found in the requested directory"
	grep -r -l $searchiteam $directory
	fi
fi

if [ "$feedback2" == "2" ]
	then
	if [ "$feedback3" == "1" ] #current directory
        then echo "Here are the files named $searchiteam found in the current directory."
        find $pwd -name $searchiteam
        fi

        if [ "$feedback3"  == "2" ] #Root Directory
        then echo "Here are the files named $searchiteam found in the root directory"
        find ~ -name $searchiteam
        fi

        if [ "$feedback3" == "3" ] #requested directory
        then echo "Here are the files named $searchiteam found in the requested directory"
        find $directory -name $searchiteam
        fi
fi
