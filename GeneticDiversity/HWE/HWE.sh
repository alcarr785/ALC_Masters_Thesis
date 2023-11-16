
for pop in "NEH" "FI" "CT" "GH" "NR" "MV";
do
	total=$(tail -n +2 "$pop.hwe" | wc -l)
	HWE_sig=$(awk 'NR > 1 && $9<0.05 {count++} END {print count}' "$pop.hwe")
	percentage=$(echo "scale=3; $HWE_sig / $total" | bc)
	echo "$pop	$percentage" >>HWE.txt
done
