"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[1176],{3905:(e,t,a)=>{a.d(t,{Zo:()=>p,kt:()=>u});var r=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},o=Object.keys(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var l=r.createContext({}),d=function(e){var t=r.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},p=function(e){var t=d(e.components);return r.createElement(l.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},h=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,o=e.originalType,l=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),h=d(a),u=n,m=h["".concat(l,".").concat(u)]||h[u]||c[u]||o;return a?r.createElement(m,i(i({ref:t},p),{},{components:a})):r.createElement(m,i({ref:t},p))}));function u(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=a.length,i=new Array(o);i[0]=h;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s.mdxType="string"==typeof e?e:n,i[1]=s;for(var d=2;d<o;d++)i[d]=a[d];return r.createElement.apply(null,i)}return r.createElement.apply(null,a)}h.displayName="MDXCreateElement"},9364:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>c,frontMatter:()=>o,metadata:()=>s,toc:()=>d});var r=a(7462),n=(a(7294),a(3905));const o={sidebar_label:"Delegated Head",sidebar_position:4},i="Delegated Head Network",s={unversionedId:"delegated-head/index",id:"delegated-head/index",title:"Delegated Head Network",description:"This document details the behaviour of so-called Delegated Hydra Head Network.",source:"@site/topologies/delegated-head/index.md",sourceDirName:"delegated-head",slug:"/delegated-head/",permalink:"/head-protocol/fr/topologies/delegated-head/",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/topologies/delegated-head/index.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_label:"Delegated Head",sidebar_position:4},sidebar:"defaultSidebar",previous:{title:"Basic Hydra Head",permalink:"/head-protocol/fr/topologies/basic/"},next:{title:"Managed Hydra Head",permalink:"/head-protocol/fr/topologies/managed/"}},l={},d=[{value:"Summary",id:"summary",level:2},{value:"Use Cases",id:"use-cases",level:2}],p={toc:d};function c(e){let{components:t,...o}=e;return(0,n.kt)("wrapper",(0,r.Z)({},p,o,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"delegated-head-network"},"Delegated Head Network"),(0,n.kt)("p",null,"This document details the behaviour of so-called ",(0,n.kt)("em",{parentName:"p"},"Delegated Hydra Head Network"),"."),(0,n.kt)("h2",{id:"summary"},"Summary"),(0,n.kt)("p",null,"A ",(0,n.kt)("em",{parentName:"p"},"Delegated Hydra Head Network")," comprises the following type of actors:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("em",{parentName:"li"},"Operator")," nodes with low expected downtime, probably operated by some company or organisation with enough resources to host this service,"),(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("em",{parentName:"li"},"Client")," nodes, either DApp instances, or mobile/personal wallets, which might not be always online and possibly can come and go.")),(0,n.kt)("p",{align:"center"},(0,n.kt)("img",{src:a(423).Z,alt:"Delegated Hydra Head",height:400})),(0,n.kt)("p",null,"Client nodes want to be able to interact with each other efficiently, at a low cost, using standard Cardano (L1) transactions. They are willing to trust ",(0,n.kt)("em",{parentName:"p"},"at least one")," ",(0,n.kt)("em",{parentName:"p"},"Operator")," to run a full Hydra and Cardano node on their behalf, in effect trading some trust for efficiency. They interact with other ",(0,n.kt)("em",{parentName:"p"},"clients")," in a single head using the Hydra ",(0,n.kt)("a",{parentName:"p",href:"https://hydra.family/head-protocol/api-reference"},"API")," and retain ownership of signing keys for transactions submitted to the Head."),(0,n.kt)("p",null,"Client nodes, may come and go offline without hampering progress of the Hydra Head. The safety of their funds rely on having at least one honest ",(0,n.kt)("em",{parentName:"p"},"Operator")," node with whom they can interact but importantly, they do not relinquish keys for spending funds inside the Head."),(0,n.kt)("p",null,"Operator nodes hold the ",(0,n.kt)("em",{parentName:"p"},"Hydra keys")," used by the protocol to sign snapshots, and the ",(0,n.kt)("em",{parentName:"p"},"Cardano keys")," used to progress the Head State Machine on L1. Each of them can sport 100s of client connections through (possibly short lived) ",(0,n.kt)("em",{parentName:"p"},"WebSocket")," connections."),(0,n.kt)("h2",{id:"use-cases"},"Use Cases"),(0,n.kt)("p",null,"This deployment model has the undesirable property of requiring trust from clients to operators and custodianship of funds committed to the Head. In effect, it's a simple way to create a ",(0,n.kt)("em",{parentName:"p"},"Side-chain-a-la-carte")," with the ",(0,n.kt)("em",{parentName:"p"},"Operators"),' being responsible for its safety and liveness. Such "alternative chains" can be easily created and deployed in an ad-hoc way without requiring the complexity associated with "classic" Side-chains.'),(0,n.kt)("p",null,"This deployment model could be interesting in scenarios where:"),(0,n.kt)("ol",null,(0,n.kt)("li",{parentName:"ol"},"The Hydra nodes can be fully trusted, either because of their identity, reputation, or lack of personal interest in the purpose of the Head,"),(0,n.kt)("li",{parentName:"ol"},"There's a need to scale the involved parties to the 100s or 1000s, something which is not possible on a normal head.")))}c.isMDXComponent=!0},423:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/delegated-head-e5019ed89664bd972890b2734d67db94.png"}}]);