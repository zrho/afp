cd ..
cabal build aibenchmark
cd aibenchmark
echo "Generating statistics..."
../dist/build/aibenchmark/aibenchmark immov Hard 50 > immov_hard.txt
../dist/build/aibenchmark/aibenchmark immov Medium 50 > immov_medium.txt
../dist/build/aibenchmark/aibenchmark immov Easy 50 > immov_easy.txt
../dist/build/aibenchmark/aibenchmark mov Hard 50 > mov_hard.txt
../dist/build/aibenchmark/aibenchmark mov Medium 50 > mov_medium.txt
../dist/build/aibenchmark/aibenchmark mov Easy 50 > mov_easy.txt
echo "Done."

