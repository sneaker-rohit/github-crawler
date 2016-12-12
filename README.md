This repo serves as the example of using [Haskell Github](https://github.com/phadej/github) API to connect
to Github API v3.

# How to build

```
stack build
```

# How to test
## Run server

```
stack exec github-api-exe
```

## Test server
We can use [Postman](https://www.getpostman.com/) with more features or simply use curl.
```
curl -X POST -H "Content-Type: application/json" -H "Cache-Control: no-cache" -d '[
   {
      "repo_name":"servant-api",
      "owner":"vn09",
      "token":""
   },
   {
      "repo_name":"awesome-courses",
      "owner":"prakhar1989",
      "token":""
   }
]' "http://localhost:8080/post-repo-info"
```

# Resoucessss
* [UPenn course](http://www.seas.upenn.edu/~cis194/fall16/)

