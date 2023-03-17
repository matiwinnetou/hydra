"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[3415],{3905:(e,t,a)=>{a.d(t,{Zo:()=>s,kt:()=>h});var r=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function o(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function d(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},l=Object.keys(e);for(r=0;r<l.length;r++)a=l[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(r=0;r<l.length;r++)a=l[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var i=r.createContext({}),p=function(e){var t=r.useContext(i),a=t;return e&&(a="function"==typeof e?e(t):o(o({},t),e)),a},s=function(e){var t=p(e.components);return r.createElement(i.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},u=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,l=e.originalType,i=e.parentName,s=d(e,["components","mdxType","originalType","parentName"]),u=p(a),h=n,m=u["".concat(i,".").concat(h)]||u[h]||c[h]||l;return a?r.createElement(m,o(o({ref:t},s),{},{components:a})):r.createElement(m,o({ref:t},s))}));function h(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var l=a.length,o=new Array(l);o[0]=u;var d={};for(var i in t)hasOwnProperty.call(t,i)&&(d[i]=t[i]);d.originalType=e,d.mdxType="string"==typeof e?e:n,o[1]=d;for(var p=2;p<l;p++)o[p]=a[p];return r.createElement.apply(null,o)}return r.createElement.apply(null,a)}u.displayName="MDXCreateElement"},7663:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>i,contentTitle:()=>o,default:()=>c,frontMatter:()=>l,metadata:()=>d,toc:()=>p});var r=a(7462),n=(a(7294),a(3905));const l={sidebar_position:99},o="Haskell Packages",d={unversionedId:"haskell_packages",id:"haskell_packages",title:"Haskell Packages",description:"The Hydra project is divided into several Haskell packages fulfilling different parts of the protocol. While some packages are internal and specific to the Hydra project, some are quite generic and may be useful to other projects facing similar issues. Regardless, we expose Haddock documentation for all of them.",source:"@site/docs/haskell_packages.md",sourceDirName:".",slug:"/haskell_packages",permalink:"/head-protocol/docs/haskell_packages",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/docs/haskell_packages.md",tags:[],version:"current",sidebarPosition:99,frontMatter:{sidebar_position:99},sidebar:"defaultSidebar",previous:{title:"Running",permalink:"/head-protocol/docs/tutorial/using_hydra/using-hydra-part-3"}},i={},p=[{value:"Public Packages",id:"public-packages",level:2},{value:"Internal Packages",id:"internal-packages",level:2}],s={toc:p};function c(e){let{components:t,...a}=e;return(0,n.kt)("wrapper",(0,r.Z)({},s,a,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"haskell-packages"},"Haskell Packages"),(0,n.kt)("p",null,"The Hydra project is divided into several Haskell packages fulfilling different parts of the protocol. While some packages are internal and specific to the Hydra project, some are quite generic and may be useful to other projects facing similar issues. Regardless, we expose ",(0,n.kt)("a",{parentName:"p",href:"https://www.haskell.org/haddock/"},"Haddock")," documentation for all of them. "),(0,n.kt)("h2",{id:"public-packages"},"Public Packages"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:null},"Package"),(0,n.kt)("th",{parentName:"tr",align:null},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/plutus-merkle-tree/index.html"},"plutus-merkle-tree")),(0,n.kt)("td",{parentName:"tr",align:null},"Implementation of Merkle Trees, compatible with on-chain Plutus validators.")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/plutus-cbor/index.html"},"plutus-cbor")),(0,n.kt)("td",{parentName:"tr",align:null},"Implementation of CBOR encoders, compatible with on-chain Plutus validators.")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-prelude/index.html"},"hydra-prelude")),(0,n.kt)("td",{parentName:"tr",align:null},"Custom Hydra Prelude used across other Hydra packages.")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-cardano-api/index.html"},"hydra-cardano-api")),(0,n.kt)("td",{parentName:"tr",align:null},"A wrapper around the ",(0,n.kt)("inlineCode",{parentName:"td"},"cardano-api"),", with era-specialized types and extra utilities.")))),(0,n.kt)("h2",{id:"internal-packages"},"Internal Packages"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:null},"Package"),(0,n.kt)("th",{parentName:"tr",align:null},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-node/index.html"},"hydra-node")),(0,n.kt)("td",{parentName:"tr",align:null},"The Hydra node.")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-node/tests/index.html"},"hydra-node tests")),(0,n.kt)("td",{parentName:"tr",align:null},"The Hydra node test code.")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-tui/index.html"},"hydra-tui")),(0,n.kt)("td",{parentName:"tr",align:null},"Terminal User Interface (TUI) for managing a Hydra node")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-plutus/index.html"},"hydra-plutus")),(0,n.kt)("td",{parentName:"tr",align:null},"Hydra Plutus Contracts")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("a",{parentName:"td",href:"https://hydra.family/head-protocol/haddock/hydra-cluster/index.html"},"hydra-cluster")),(0,n.kt)("td",{parentName:"tr",align:null},"Integration test suite using a local cluster of Cardano and hydra nodes")))))}c.isMDXComponent=!0}}]);