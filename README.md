# comp-scaling

## Description of data-files:

### US data:

#### Data fields:

* Year: Year column.
* ASJC.4D: Academic field column. 
* Occ.3D: Occupation column, at the three digit level.
* Class: Patent technology class column.

#### Files:

##### Region-Activity-Year:

##### Activity-Year:

fields-years.csv
```
Primary Key: ASJC.4D,Year

Fields:
Nb.Papers: Number of papers
Mean.Nb.Aut: Average number of authors per paper
```

occs-years.csv
```
Primary Key: Occ.3D,Year

Fields:
Nb.Emp: Number of workers
Mean.Y.Educ: Average years of schooling
```


##### Activity:

fields.csv
```
Primary Key: ASJC.4D

Fields:
ASJC.4D.Name: Academic field name
ASJC.2D: Academic field at the 2 digit level
ASJC.2D.Name: Name of the academic field at the 2 digit level
Main: ???
```

inds.csv
```
Primary Key: NAICS.4D

Fields:
NAICS.4D.Name: Name of the industry
NAICS.2D: Industry code at the 2 digit level
NAICS.2D.Name: Name of the industry at the 2 digit level
```

occs.csv
```
Primary Key: Occ.3D

Fields:
Occ.3D.Name: Name of occupation
Occ.2D: Occupation code at the two digit level
Occ.2D.Name: Occupation name at the two digit level
```

techs.csv
```
Primary Key: Class

Fields:
Class.Name: Name of technology class.
Original.USPC.Class: ???
Date.Established.Class: ???
NBER.Sub.Cat: ???
NBER.Sub.Cat.Name: ???
NBER.Cat: ???
NBER.Cat.Name: ???
```


### Brazilian industries and occupations:

#### Data fields:

* year: Year column.
* rcode: Column indicating the microregion.
* icode3: Industry column, at the three digit level (CNAE).
* ocode3: Occupation column, at the three digit level (CBO).

#### Files:

##### Region-Activity-Year:

BRA_RegIndYr.csv
```
Primary Key: year,rcode,icode3

Fields:
no_people: Number of people
```

BRA_RegOccYr.csv
```
Primary Key: year,rcode,ocode3

Fields:
no_people: Number of people
```

##### Activity-Year:

BRA_IndYr.csv
```
Primary Key: year,icode3

Fields:
no_people: Number of workers
edu: Average years of schooling
avg_wage: Average monthly wage
```

BRA_OccYr.csv
```
Primary Key: year,ocode3

Fields:
no_people: Number of workers
edu: Average years of schooling
avg_wage: Average monthly wage
```

BRA_RegYr.csv
```
Primary Key: year,rcode

Fields:
no_people: Number of workers
gdp: GDP
gdp_pc: GDP per capita
pop: Population
```


##### Activity:

BRA_Ind.csv
```
Primary Key: icode3

Fields:
icode2: Industry code at the 2 digit level
icode1: Industry code at the 1 digit level
iname: Industry name in portuguese
```

BRA_Occ.csv
```
Primary Key: ocode3

Fields:
ocode2: Occupation code at the 2 digit level
ocode1: Occupation code at the 1 digit level
oname: Occupation name in portuguese
```

BRA_Reg.csv
```
Primary Key: rcode

Fields:
rcode_meso: Code for the corresponding mesoregion
state: State code
rname: Name of the microregion
area: Total area of the microregion
capital: Dummy variable indicating whether the state capital is located in this microregion
lat: Latitude of the microregion
lon: Longitude of the microregion
```
