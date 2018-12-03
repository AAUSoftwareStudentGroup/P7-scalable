echo "$(curl -s https://randomuser.me/api/?format=csv\&results=$1\&inc=email,name,gender,dob,picture)" | sed 1d | while IFS=, read -r gender name_title name_first name_last email dob_date dob_age picture_large picture_medium picture_thumbnail
do
    birthdate=$(echo $dob_date| cut -d'T' -f 1)
    gender="$(tr '[:lower:]' '[:upper:]' <<< ${gender:0:1})${gender:1}"
    echo -e " $name_first\t|$name_last\t|$gender\t|$email\t|$birthdate|$picture_large"
    
    # Get profile picture
    picture=$(curl -s $picture_large --output - | base64 -w 0)
    # echo "curl -s $picture_large --output - | base64"
    # echo "{
    #   \"email\":\"$email\",
    #   \"password\":\"123\",
    #   \"username\":\"$name_first $name_last\",
    #   \"gender\":\"$gender\",
    #   \"birthday\":\"$birthdate\",
    #   \"town\":\"TownTown\",
    #   \"profileText\":\"ProfileText\",
    #   \"imageData\":\"${picture}\"
    #   }"
    # Create user
    curl -X POST \
    http://api.dating.local/users \
    -H 'Content-Type: application/json' \
    -H 'cache-control: no-cache' \
    --data-binary "{
      \"email\":\"$email\",
      \"password\":\"123\",
      \"username\":\"$name_first $name_last\",
      \"gender\":\"$gender\",
      \"birthday\":\"$birthdate\",
      \"town\":\"TownTown\",
      \"profileText\":\"ProfileText\",
      \"imageData\":\"${picture}\"
      }" --compressed
      echo ""
done 

