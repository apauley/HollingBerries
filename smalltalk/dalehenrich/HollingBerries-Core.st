"load CSVParser by Thomas Cleenewerck"
Gofer new
  squeaksource: 'CSVParser';
  package: 'CSV';
  load!

"patch CSVParser bug"!
!CSVParser methodsFor: 'as yet unclassified' stamp: 'dkh 5/20/2012 18:02'!
initializeWithStream: aStream
	self initialize.
	stream := aStream! !

"HollingBerries Source"!
Object subclass: #HBPriceListGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBPriceListGenerator methodsFor: 'generator' stamp: 'dkh 5/20/2012 20:24'!
formatDescription: description on: aStream
	aStream nextPutAll: (String streamContents: [ :s | s nextPutAll: description ] limitedTo: 31)! !

!HBPriceListGenerator methodsFor: 'generator' stamp: 'dkh 5/20/2012 20:23'!
formatSellByDate: sellByDate on: aStream
	sellByDate printOn: aStream format: #(3 2 1 $/ 1 1 2)! !

!HBPriceListGenerator methodsFor: 'generator' stamp: 'dkh 5/20/2012 20:20'!
formatSellingPrice: sellingPrice on: aStream
	| priceString |
	aStream nextPut: $R.
	priceString := String streamContents: [ :s | sellingPrice / 100 printOn: s showingDecimalPlaces: 2 ].
	8 - priceString size timesRepeat: [ aStream space ].
	aStream nextPutAll: priceString! !

!HBPriceListGenerator methodsFor: 'generator' stamp: 'dkh 5/20/2012 20:36'!
generatePriceListFrom: csvCollection on: aStream
	2 to: csvCollection size do: [ :index | 
		| product |
		product := HBProduct fromCsv: (csvCollection at: index).
		product unitCount
			timesRepeat: [ 
				self
					formatSellingPrice: product sellingPrice on: aStream;
					formatSellByDate: product sellByDate on: aStream;
					formatDescription: product description on: aStream.
				aStream cr ] ]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBPriceListGenerator class
	instanceVariableNames: ''!

!HBPriceListGenerator class methodsFor: 'generator' stamp: 'dkh 5/20/2012 20:29'!
generatePriceList: priceFilePath from: csvFilePath
	"self generatePriceList: '/opt/git/HollingBerries/smalltalk_pricefile.txt' from: '/opt/git/HollingBerries/produce.csv'"

	| csvCollection directory |
	FileStream
		fileNamed: csvFilePath
		do: [ :fileStream | 
			csvCollection := CSVParser parse: fileStream.
			directory := fileStream directory ].
	FileStream
		forceNewFileNamed: priceFilePath
		do: [ :fileStream | 
			fileStream lineEndConvention: #lf.
			self new generatePriceListFrom: csvCollection on: fileStream ]! !


Object subclass: #HBProduct
	instanceVariableNames: 'supplierId productCode description deliveryDate costPrice unitCount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBProduct methodsFor: 'business rules' stamp: 'dkh 5/20/2012 16:07'!
costPrice
	^ costPrice! !

!HBProduct methodsFor: 'business rules' stamp: 'dkh 5/20/2012 16:07'!
deliveryDate
	^ deliveryDate! !

!HBProduct methodsFor: 'business rules' stamp: 'dkh 5/20/2012 17:10'!
markup
	^ 1.50! !

!HBProduct methodsFor: 'business rules' stamp: 'dkh 5/20/2012 18:39'!
sellByDate
	^ self supplier sellByDateFor: self! !

!HBProduct methodsFor: 'business rules' stamp: 'dkh 5/20/2012 17:48'!
sellByDuration
	^ 1 weeks! !

!HBProduct methodsFor: 'business rules' stamp: 'dkh 5/20/2012 20:37'!
sellingPrice
	^ self supplier sellingPriceFor: self! !


!HBProduct methodsFor: 'initialization' stamp: 'dkh 5/20/2012 20:51'!
initializeFromCsv: csvLine
	description := csvLine at: 3.
	deliveryDate := Date readFrom: (csvLine at: 4) readStream pattern: 'yyyy/mm/dd'.
	costPrice := (csvLine at: 5) asInteger.
	unitCount := (csvLine at: 6) asInteger.
	supplierId := (csvLine at: 1) asInteger! !


!HBProduct methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:07'!
description
	^ description! !

!HBProduct methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:08'!
productCode
	^ productCode! !

!HBProduct methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:08'!
productCode: anObject
	productCode := anObject! !

!HBProduct methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:09'!
supplier
	^ HBSupplier from: self supplierId! !

!HBProduct methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:08'!
supplierId
	^ supplierId! !

!HBProduct methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:08'!
unitCount
	^ unitCount! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBProduct class
	instanceVariableNames: ''!

!HBProduct class methodsFor: 'lookup' stamp: 'dkh 5/20/2012 20:43'!
lookupProduct: productCode
	| productClasses |
	productClasses := OrderedCollection new.
	self subclasses do: [ :productCategory | productClasses addAll: productCategory subclasses ].
	productClasses addAll: self subclasses.
	^ (productClasses
		detect: [ :productClass | productClass acceptsProductId: productCode ]
		ifNone: [ self error: 'Product id out of range' ]) new productCode: productCode! !


!HBProduct class methodsFor: 'instance creation' stamp: 'dkh 5/20/2012 20:52'!
fromCsv: csvLine
	^ (self lookupProduct: (csvLine at: 2) asInteger) initializeFromCsv: csvLine! !


HBProduct subclass: #HBFruit
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBFruit class
	instanceVariableNames: ''!

!HBFruit class methodsFor: 'testing' stamp: 'dkh 5/20/2012 18:28'!
acceptsProductId: productCode
	^ 1000 <= productCode and: [ productCode <= 1999 ]! !


HBFruit subclass: #HBApple
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBApple methodsFor: 'accessing' stamp: 'dkh 5/20/2012 17:11'!
markup
	^ 1.40! !

!HBApple methodsFor: 'accessing' stamp: 'dkh 5/20/2012 17:48'!
sellByDuration
	^ 2 weeks! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBApple class
	instanceVariableNames: ''!

!HBApple class methodsFor: 'testing' stamp: 'dkh 5/20/2012 18:29'!
acceptsProductId: productCode
	^ 1100 <= productCode and: [ productCode <= 1199 ]! !


HBFruit subclass: #HBBerry
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBBerry methodsFor: 'accessing' stamp: 'dkh 5/20/2012 17:11'!
markup
	^ 1.55! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBBerry class
	instanceVariableNames: ''!

!HBBerry class methodsFor: 'testing' stamp: 'dkh 5/20/2012 18:29'!
acceptsProductId: productCode
	^ 1300 <= productCode and: [ productCode <= 1399 ]! !


HBFruit subclass: #HBBanana
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBBanana methodsFor: 'accessing' stamp: 'dkh 5/20/2012 17:11'!
markup
	^ 1.35! !

!HBBanana methodsFor: 'accessing' stamp: 'dkh 5/20/2012 17:48'!
sellByDuration
	^ 5 days! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBBanana class
	instanceVariableNames: ''!

!HBBanana class methodsFor: 'testing' stamp: 'dkh 5/20/2012 18:29'!
acceptsProductId: productCode
	^ 1200 <= productCode and: [ productCode <= 1299 ]! !


Object subclass: #HBSupplier
	instanceVariableNames: 'supplierId'
	classVariableNames: 'Suppliers'
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 20:32'!
sellByDateFor: product
	^ product deliveryDate + product sellByDuration! !

!HBSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 20:34'!
sellingPriceFor: product
	^ product costPrice * product markup! !


!HBSupplier methodsFor: 'accessing' stamp: 'dkh 5/20/2012 15:57'!
supplierId
	^ supplierId! !

!HBSupplier methodsFor: 'accessing' stamp: 'dkh 5/20/2012 15:57'!
supplierId: anObject
	supplierId := anObject! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBSupplier class
	instanceVariableNames: ''!

!HBSupplier class methodsFor: 'instance creation' stamp: 'dkh 5/20/2012 17:25'!
from: supplierId
	^ (self suppliers at: supplierId ifAbsent: [ HBGenericSupplier ]) new supplierId: supplierId! !



!HBSupplier class methodsFor: 'registration' stamp: 'dkh 5/20/2012 15:55'!
register
	self suppliers at: self supplierId put: self! !


!HBSupplier class methodsFor: 'accessing' stamp: 'dkh 5/20/2012 15:55'!
suppliers
	Suppliers ifNil: [ Suppliers := Dictionary new ].
	^ Suppliers! !


HBSupplier subclass: #HBTroubleSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBTroubleSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 20:32'!
sellByDateFor: product
	^ (super sellByDateFor: product) - self sellByPenalty! !

!HBTroubleSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 20:32'!
sellByPenalty
	^ 3 days! !

!HBTroubleSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 20:37'!
sellingPriceFor: product
	^ (super sellingPriceFor: product) - 200 max: 0! !


HBSupplier subclass: #HBPremiumSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

!HBPremiumSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 18:41'!
markup
	^ 0.10! !

!HBPremiumSupplier methodsFor: 'busines rules' stamp: 'dkh 5/20/2012 20:34'!
sellingPriceFor: product
	^ product costPrice * (product markup + self markup) roundUpTo: 100! !


HBPremiumSupplier subclass: #HBKarelVisserSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBKarelVisserSupplier class
	instanceVariableNames: ''!

!HBKarelVisserSupplier class methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:01'!
supplierId
	^ 204! !


!HBKarelVisserSupplier class methodsFor: 'initialization' stamp: 'dkh 5/20/2012 16:01'!
initialize
	"self initialize"

	self register! !


HBTroubleSupplier subclass: #HBTogethernessTshabalalaSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBTogethernessTshabalalaSupplier class
	instanceVariableNames: ''!

!HBTogethernessTshabalalaSupplier class methodsFor: 'accessing' stamp: 'dkh 5/20/2012 15:58'!
supplierId
	^ 101! !


!HBTogethernessTshabalalaSupplier class methodsFor: 'initialization' stamp: 'dkh 5/20/2012 15:58'!
initialize
	"self initialize"

	self register! !


HBTroubleSupplier subclass: #HBSusanWindlerSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBSusanWindlerSupplier class
	instanceVariableNames: ''!

!HBSusanWindlerSupplier class methodsFor: 'accessing' stamp: 'dkh 5/20/2012 15:53'!
supplierId
	^ 32! !


!HBSusanWindlerSupplier class methodsFor: 'initialization' stamp: 'dkh 5/20/2012 15:56'!
initialize
	"self initialize"

	self register! !


HBSupplier subclass: #HBGenericSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!


HBPremiumSupplier subclass: #HBPromiseMashanguSupplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'HollingBerries-Core'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HBPromiseMashanguSupplier class
	instanceVariableNames: ''!

!HBPromiseMashanguSupplier class methodsFor: 'accessing' stamp: 'dkh 5/20/2012 16:00'!
supplierId
	^ 219! !


!HBPromiseMashanguSupplier class methodsFor: 'initialization' stamp: 'dkh 5/20/2012 16:00'!
initialize
	"self initialize"

	self register! !

HBKarelVisserSupplier initialize!
HBTogethernessTshabalalaSupplier initialize!
HBSusanWindlerSupplier initialize!
HBPromiseMashanguSupplier initialize!


