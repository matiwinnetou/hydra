"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5642],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>m});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},c=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),d=p(n),m=r,f=d["".concat(s,".").concat(m)]||d[m]||u[m]||o;return n?a.createElement(f,i(i({ref:t},c),{},{components:n})):a.createElement(f,i({ref:t},c))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:r,i[1]=l;for(var p=2;p<o;p++)i[p]=n[p];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},8834:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>l,toc:()=>p});var a=n(7462),r=(n(7294),n(3905));const o={slug:8,title:"8. Custom Prelude\n",authors:[],tags:["Accepted"]},i=void 0,l={permalink:"/head-protocol/ja/adr/8",source:"@site/adr/2021-06-18_008-use-custom-prelude.md",title:"8. Custom Prelude\n",description:"Status",date:"2021-06-18T00:00:00.000Z",formattedDate:"2021\u5e746\u670818\u65e5",tags:[{label:"Accepted",permalink:"/head-protocol/ja/adr/tags/accepted"}],readingTime:1.68,truncated:!1,authors:[],frontMatter:{slug:"8",title:"8. Custom Prelude\n",authors:[],tags:["Accepted"]},prevItem:{title:"7. Use with-pattern based component interfaces\n",permalink:"/head-protocol/ja/adr/7"},nextItem:{title:"9. Simplify Logging\n",permalink:"/head-protocol/ja/adr/9"}},s={authorsImageUrls:[]},p=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}],c={toc:p};function u(e){let{components:t,...n}=e;return(0,r.kt)("wrapper",(0,a.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"status"},"Status"),(0,r.kt)("p",null,"Accepted"),(0,r.kt)("h2",{id:"context"},"Context"),(0,r.kt)("p",null,"In a Haskell project, we often get to use and re-use the same libraries and functions. Haskell comes with a default ",(0,r.kt)("inlineCode",{parentName:"p"},"Prelude")," package with the ",(0,r.kt)("inlineCode",{parentName:"p"},"base")," library, which provides a good and sensible starting point. However, the base ",(0,r.kt)("inlineCode",{parentName:"p"},"Prelude")," also comes with a few quirks:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Many commonly used functions or constructors are not exported by default (e.g. ",(0,r.kt)("inlineCode",{parentName:"li"},"bracket"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"foldM"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"first"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"lift"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"forM"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"when"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"SomeException"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"Set"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"&")," ...etc)."),(0,r.kt)("li",{parentName:"ul"},"Many functions in the base Prelude are partial, like ",(0,r.kt)("inlineCode",{parentName:"li"},"head")," or ",(0,r.kt)("inlineCode",{parentName:"li"},"read"),". "),(0,r.kt)("li",{parentName:"ul"},"Many functions simply happens in plain ",(0,r.kt)("inlineCode",{parentName:"li"},"IO"),", whereas applications usually try to push IO to the boundary as much as possible (for example, using mtl-style class constraints)."),(0,r.kt)("li",{parentName:"ul"},"The interface for I/O operations in the base Prelude is ",(0,r.kt)("inlineCode",{parentName:"li"},"String"),", which comes with quite major performance hit and often forces to convert back and forth to ",(0,r.kt)("inlineCode",{parentName:"li"},"Text")," or ",(0,r.kt)("inlineCode",{parentName:"li"},"ByteString")," equivalents.")),(0,r.kt)("p",null,"All-in-all, while it ",(0,r.kt)("em",{parentName:"p"},"does the job"),", the base ",(0,r.kt)("inlineCode",{parentName:"p"},"Prelude")," may not necessarily be the most ",(0,r.kt)("em",{parentName:"p"},"convenient")," prelude for an active project development. "),(0,r.kt)("h2",{id:"decision"},"Decision"),(0,r.kt)("p",null,"We'll use a custom prelude to help us get more productive and more importantly, to reduce the daily friction of our interactions with the base prelude. While ",(0,r.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/relude"},(0,r.kt)("inlineCode",{parentName:"a"},"relude"))," makes for a good candidate, we still chose to re-wrap it in a custom ",(0,r.kt)("inlineCode",{parentName:"p"},"Hydra.Prelude")," module to grant us the ability to add or remove a few things specifics to Hydra and Cardano in general. In particular, we will hide from ",(0,r.kt)("inlineCode",{parentName:"p"},"relude")," all the re-exports of the ",(0,r.kt)("a",{parentName:"p",href:"https://hackage.haskell.org/package/stm"},(0,r.kt)("inlineCode",{parentName:"a"},"stm"))," library in favor of ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/ouroboros-network/tree/e338f2cf8e1078fbda9555dd2b169c6737ef6774/io-classes"},(0,r.kt)("inlineCode",{parentName:"a"},"io-classes"))," which we already use pervasively and which provides (among other things) most of the same capabilities."),(0,r.kt)("h2",{id:"consequences"},"Consequences"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Remove uses of 'cardano-prelude' in favor of a new 'hydra-prelude' module."),(0,r.kt)("li",{parentName:"ul"},"Cleaning up of imports from existing file modules."),(0,r.kt)("li",{parentName:"ul"},"Happier feeling day after day from using a developer-friendly prelude."),(0,r.kt)("li",{parentName:"ul"},"Stop loosing time in often re-importing the same functions over and over. "),(0,r.kt)("li",{parentName:"ul"},"Have an explicit point for discouraging / blessing usage of one or the other function, as well as documenting such decisions")))}u.isMDXComponent=!0}}]);