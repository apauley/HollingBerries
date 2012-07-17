" Squeak 4.1, 4.2, 4.3 "

"
   Installer ss project: 'CSV'; install: 'CSV'.
"

| pathToCsv pathToOut keys kindOf markup sellby troubleSupplierIDs trouble 
  premiumSupplierIDs premium outStream output |

pathToCsv := 'produce.csv'.
pathToOut := 'pricefile.txt'.
keys := #(suppID prodCode prodDesc delivDate unitPrice numUnits).

kindOf := [:data |
   | pcode |
   pcode := (data at: #prodCode) asNumber.
   true caseOf: {
      [(1100 to: 1199) includes: pcode] -> [#apple].
      [(1200 to: 1299) includes: pcode] -> [#banana].
      [(1300 to: 1399) includes: pcode] -> [#berry].
      [(1000 to: 1999) includes: pcode] -> [#fruit]}].

markup := [:data |
   | rate |
   rate := (kindOf value: data) caseOf: {
      [#apple] -> [1.4s3].
      [#banana] -> [1.35s3].
      [#berry] -> [1.55s3]} otherwise: [1.5s3].
   data at: #retailPrice put: (data at: #unitPrice) * rate].

sellby := [:data |
   | after |
   after := (kindOf value: data) caseOf: {
      [#apple] -> [2 weeks].
      [#banana] -> [5 days]} otherwise: [1 week].
   data at: #sellbyDate put: (data at: #delivDate) asDate + after].

troubleSupplierIDs := #('32' '101').
trouble := [:data |
   (troubleSupplierIDs includes: (data at: #suppID)) ifTrue: [
      data at: #sellbyDate put: (data at: #sellbyDate) - 3 days.
      data at: #retailPrice put: (((data at: #retailPrice) - 2) max: 0s3)]].

premiumSupplierIDs := #('219' '204').
premium := [:data |
   (premiumSupplierIDs includes: (data at: #suppID)) ifTrue: [
      data at: #retailPrice put: (((data at: #unitPrice) * 0.1s3 + (data at: #retailPrice)) roundUpTo: 1s3)]].

outStream := nil.
output := [:data |
   | price priceStr |
   price := (data at: #retailPrice) roundTo: 0.01s3.
   priceStr := (price asString allButLast: 3) forceTo: 8 paddingStartWith: $ .
   (data at: #numUnits) asInteger timesRepeat: [
      outStream nextPutAll: 'R', priceStr.
      outStream nextPutAll: ((data at: #sellbyDate) yyyymmdd replaceAll: $- with: $/).
      outStream nextPutAll: ((data at: #prodDesc) forceTo: 31 paddingWith: $ ).
      outStream cr]].

StandardFileStream oldFileNamed: pathToCsv do: [:csvFile |
   | csv fields |
   csv := CSVParser on: csvFile.
   csv nextRow.
   StandardFileStream forceNewFileNamed: pathToOut do: [:outFile | 
      outStream := outFile.
      [(fields := csv nextRow) notEmpty] whileTrue: [
         | data |
         data := Dictionary new.
         keys with: fields do: [:key :str | data at: key put: str].
         data at: #unitPrice put: (data at: #unitPrice) / 100s3.
         {markup. sellby. trouble. premium. output} do: [:each | each value: data]].
   ]
].

(StandardFileStream fileNamed: pathToOut) edit
