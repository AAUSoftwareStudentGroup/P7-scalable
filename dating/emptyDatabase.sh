echo "DELETE FROM users WHERE id > -1;" | PGPASSWORD=datingdbpassword psql -h localhost -d datingdb -U datingdbuser
curl -X POST \
http://api.dating.local:8002/users \
-H 'Content-Type: application/json' \
-H 'cache-control: no-cache' \
-d "{
   \"email\":\"tanner@helland.ninja\",
   \"password\":\"123\",
   \"username\":\"th\",
   \"gender\":\"Male\",
   \"birthday\":\"0001-01-01\",
   \"town\":\"TannerTown\",
   \"profileText\":\"Have you tried my RGB to colour temperature algorithm!?\",
   \"authToken\":\"token\"
}"
