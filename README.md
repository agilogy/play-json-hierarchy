# play-json hierarchy format

## Installation

```
resolvers += "Agilogy GitLab" at "https://gitlab.com/api/v4/groups/583742/-/packages/maven"

libraryDependencies += "com.agilogy" %% "play-json-hierarchy" % "0.2"
```

## Publishing

To publish this package to Agilogy's Package Registry, set the `GITLAB_DEPLOY_TOKEN` environment variable and then run the following command in sbt:

```
sbt:play-json-hierarchy> +publish
```

## Copyright

Copyright 2015 Agilogy

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the 
License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an 
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific 
language governing permissions and limitations under the License.
