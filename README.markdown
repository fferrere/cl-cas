# CL-CAS

Library to help [CAS authenticaton](https://apereo.github.io/cas/6.0.x/protocol/CAS-Protocol.html) to Common Lisp web applications.

Supports 2 modes of authentication (See CAS Protocol Specs) :
- Default : Single sign-on (SSO) mode 
- Renew : Force authentication (SSO Off)

Supports partialy CAS Protocol V2 specs.

## Installation
1. git clone https://github.com/fferrere/cl-cas
2. Add cl-cas system to your project (asd file) `:depends-on ("cl-cas" ...)`


## Usage

- Initialize a CAS-Client object :
  - Default (with sso) : `(make-instance 'cas-client :server-url "https://cas.example.org/cas")`
  - Force authentication : `(make-instance 'cas-client :server-url "https://cas.example.org/cas" :renew t)`
- Use above functions

### Function cas-login-url

Returns CAS Server login URL

Parameters :
- cas-client : a cas-client instance
- service : URL of the Web app

Example :
```
CL-USER> (let ((cas (make-instance 'cas-client :server-url "https://cas.example.org/cas")))
           (cl-cas:cas-login-url cas "http://www.example.org/service"))

https://cas.example.org/cas/login?service=http%3A%2F%2Fwww.example.org%2Fservice
```

### Function cas-ticket

Extracts CAS ticket value from HTTP 'Query String'

Example :
HTTP URL : `https://cas.example.org/cas/validate?service=http%3A%2F%2Fwww.example.org%2Fservice&ticket=ST-1856339-aA5Yuvrxzpv8Tau1cYQ7`

```
CL-USER> (cl-cas:cas-ticket "service=http%3A%2F%2Fwww.example.org%2Fservice&ticket=ST-1856339-aA5Yuvrxzpv8Tau1cYQ7")

ST-1856339-aA5Yuvrxzpv8Tau1cYQ7
```

### Function cas-service-validate

Checks the validity of a service ticket and returns :
- On ticket validation success : user name and attributes.
- On ticket validation failure : nil.

Parameters :
- cas-client : a cas-client instance
- ticket : cas ticket (string)
- format : either :xml (défaut), or :json

```
CL-USER> (let ((cas (make-instance 'cas-client :server-url "https://cas.example.org/cas")))
           (cl-cas:cas-service-validate cas "http://www.example.org/service" "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7"))

<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>
    <cas:authenticationSuccess>
        <cas:user>username</cas:user>
        </cas:authenticationSuccess>
</cas:serviceResponse>
```

```
CL-USER> (let ((cas (make-instance 'cas-client :server-url "https://cas.example.org/cas")))
           (cl-cas:cas-service-validate cas "http://www.example.org/service" "ST-1856339-aA5Yuvrxzpv8Tau1cYQ7" :format :json))

{
  "serviceResponse" : {
    "authenticationSuccess" : {
      "user" : "username",
      "attributes" : { }
    }
  }
}
```

### Function cas-logout-url

Returns CAS Server logout URL.

Parameters :
- cas-client : a cas-client instance
- [Optional] url : if specified, should be on the logout page with descriptive text (if supported by the CAS server).

Example :
```
CL-USER> (let ((cas (make-instance 'cas-client :server-url "https://cas.example.org/cas")))
           (cl-cas:cas-logout-url cas "http://www.example.org/logout"))

https://cas.example.org/cas/logout?url=http%3A%2F%2Fwww.example.org%2Flogout
```
## Demo

See https://github.com/fferrere/cas-demo

## References
- https://apereo.github.io/cas/6.0.x/protocol/CAS-Protocol.html
- https://apereo.github.io/cas/6.0.x/protocol/CAS-Protocol-V2-Specification.html

## Author

* Frédéric FERRERE (frederic.ferrere@gmail.com)

## Licence

Apache-2.0 (https://www.apache.org/licenses/LICENSE-2.0)


