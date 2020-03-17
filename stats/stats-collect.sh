echo 'commit;allocations;memory;time' > stats.csv
for h in $(git log 1657ce0^..1d68425 --format=%H|tac)
do
  git log -n 1 --oneline $h
  test -f stats/$h.txt && echo "$(echo $h|cut -c-7);$(./tally.pl < stats/$h.txt)" | tee -a stats.csv
done
