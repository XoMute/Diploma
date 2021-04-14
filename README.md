# Examples:

## Json: 
```javascript
{
  "name1": true,
  "name2": [
    1.0,
    2.0,
    3.0
  ],
  "name3": {
    "name4": [
      null,
      "Some timestamp",
      {
        "name5": "Value"
      }
    ]
  }
}
```

### Query:
`".name2"`
### Result:
```javascript
[
  1.0,
  2.0,
  3.0
]
```
### Query:
`".name2[1:3]"`
### Result:
```javascript
2.0
3.0
```
### Query:
`".name3.name4[1], name3.name4[2].name5"`
### Result:
```javascript
"Some timestamp"
"Value"
```
## Json: 
```javascript
[
  {
    "foo": {
      "bar": {
        "buz": 1.0
      }
    }
  },
  {
    "foo": {
      "bar": {
        "buz": 2.0
      }
    }
  },
  {
    "foo": {
      "bar": {
        "buz": 3.0
      }
    }
  }
]
```
### Query:
`".[]"`
### Result:
```javascript
{
  "foo": {
    "bar": {
      "buz": 1.0
    }
  }
}
{
  "foo": {
    "bar": {
      "buz": 2.0
    }
  }
}
{
  "foo": {
    "bar": {
      "buz": 3.0
    }
  }
}
```
### Query:
`".[] | .foo"`
### Result:
```javascript
{
  "bar": {
    "buz": 1.0
  }
}
{
  "bar": {
    "buz": 2.0
  }
}
{
  "bar": {
    "buz": 3.0
  }
}
```
### Query:
`".[] | .foo | .bar | .buz"` or `".[] | .foo.bar.buz"`
### Result:
```javascript
1.0
2.0
3.0
```
### Query:
`".[] | .foo.bar.buz, .foo.bar"`
### Result:
```javascript
1.0
{
  "buz": 1.0
}
2.0
{
  "buz": 2.0
}
3.0
{
  "buz": 3.0
}
```
### Query:
`".[] | .foo.bar.buz > 1"`
### Result:
```javascript
{
  "foo": {
    "bar": {
      "buz": 2.0
    }
  }
}
{
  "foo": {
    "bar": {
      "buz": 3.0
    }
  }
}
```
### Query:
`".[] | .foo.bar.buz > 1.0e0 | .foo.bar.buz, .foo.bar"`
### Result:
```javascript
2.0
{
  "buz": 2.0
}
3.0
{
  "buz": 3.0
}
```
