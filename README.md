HN Scraper
=============

A Scraper for http://hackerstreet.in/ written in Clojure

Usage
-----

fetch-stories function returns a lazy seq of stories  
fetching stories might span multiple pages. (Will automatically follow the more link)  
Each story is returned as a structmap with following keys  
* :title - Title of the story  
* :permalink - Permalink of the story  
* :score - score of submission  
* :num-comments - number of comments made  
* :sub-time - Time since submission in seconds  
* :user - user who submitted the story  
* :id - Story id  
* :discussion - link to story discussion thread  
* :domain - domain of the story submitted  

```clojure
;to fetch top 35 stories on site
(take 35 (fetch-stories "http://hackerstreet.in/"))
```

License
-------

Copyright &copy; 2012 Praveen Kumar Telugu  
Licensed under the Apache License, Version 2.0 (the "License");  
you may not use this file except in compliance with the License.  
You may obtain a copy of the License at  

    http://www.apache.org/licenses/LICENSE-2.0  

Unless required by applicable law or agreed to in writing, software  
distributed under the License is distributed on an "AS IS" BASIS,  
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  
See the License for the specific language governing permissions and  
limitations under the License.  
