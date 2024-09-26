wget -i hector-facts.urls.txt

for file in *.zip; do
    unzip "$file" && rm "$file"
done


