echo "$(curl https://randomuser.me/api/?format=csv\&results=$1\&inc=email,name,gender,dob)" | sed 1d | while IFS=, read -r gender name_title name_first name_last email dob_date dob_age
do
    birthdate=$(echo $dob_date| cut -d'T' -f 1)
    gender="$(tr '[:lower:]' '[:upper:]' <<< ${gender:0:1})${gender:1}"
    echo -e " $name_first\t|$name_last\t|$gender\t|$email\t|$birthdate"
    curl -X POST \
    http://api.dating.local:8002/users \
    -H 'Content-Type: application/json' \
    -H 'cache-control: no-cache' \
    -d "{
       \"email\":\"$email\",
       \"password\":\"adminadmin\",
       \"username\":\"$name_first $name_last\",
       \"gender\":\"$gender\",
       \"birthday\":\"$birthdate\",
       \"town\":\"TownTown\",
       \"profileText\":\"ProfileText\",
       \"authToken\":\"token\"
    }"
done 


