This repo serves as the example of using [Haskell Github](https://github.com/phadej/github) API to connect
to Github API v3.

# How to build

```
stack build
```

# How to test
## Github Token
Currently, Github limits [Rate limit](https://developer.github.com/v3/#rate-limiting) for unauthenticated user is 60 requests / hour. To get more request, we need to make request using Basic Authentication or OAuth, we can make up to 5000 requests per hour. In this program, we tend to use OAuth request.

If you don't have token, you can go to [Token page](https://github.com/settings/tokens) to generate the new one.

Copy file `_config.conf` to `config.conf` and config your OAuth token here.
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

# Resouces
* [UPenn course](http://www.seas.upenn.edu/~cis194/fall16/)

