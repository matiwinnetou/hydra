"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[7742],{3905:(e,t,a)=>{a.d(t,{Zo:()=>l,kt:()=>u});var r=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},o=Object.keys(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var p=r.createContext({}),c=function(e){var t=r.useContext(p),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},l=function(e){var t=c(e.components);return r.createElement(p.Provider,{value:t},e.children)},d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,o=e.originalType,p=e.parentName,l=s(e,["components","mdxType","originalType","parentName"]),m=c(a),u=n,h=m["".concat(p,".").concat(u)]||m[u]||d[u]||o;return a?r.createElement(h,i(i({ref:t},l),{},{components:a})):r.createElement(h,i({ref:t},l))}));function u(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=a.length,i=new Array(o);i[0]=m;var s={};for(var p in t)hasOwnProperty.call(t,p)&&(s[p]=t[p]);s.originalType=e,s.mdxType="string"==typeof e?e:n,i[1]=s;for(var c=2;c<o;c++)i[c]=a[c];return r.createElement.apply(null,i)}return r.createElement.apply(null,a)}m.displayName="MDXCreateElement"},6409:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>d,frontMatter:()=>o,metadata:()=>s,toc:()=>c});var r=a(7462),n=(a(7294),a(3905));const o={},i="Poker Game",s={unversionedId:"poker-game/index",id:"poker-game/index",title:"Poker Game",description:"A prototypical example of a multi-party state channel.",source:"@site/use-cases/poker-game/index.md",sourceDirName:"poker-game",slug:"/poker-game/",permalink:"/head-protocol/use-cases/poker-game/",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/use-cases/poker-game/index.md",tags:[],version:"current",frontMatter:{},sidebar:"defaultSidebar",previous:{title:"Overview",permalink:"/head-protocol/use-cases/"},next:{title:"Pay-per-use API",permalink:"/head-protocol/use-cases/pay-per-use-api/"}},p={},c=[],l={toc:c};function d(e){let{components:t,...o}=e;return(0,n.kt)("wrapper",(0,r.Z)({},l,o,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"poker-game"},"Poker Game"),(0,n.kt)("blockquote",null,(0,n.kt)("p",{parentName:"blockquote"},"A prototypical example of a multi-party state channel.")),(0,n.kt)("p",null,"We love to explain how Hydra heads work using the analogy of a ",(0,n.kt)("em",{parentName:"p"},"poker game")," because it suits pretty well the fundamentals behind the Head protocol. A poker game (or any game in general) is a situation with a clear beginning and an end that evolves around a set of agreed-upon rules. In the case of a poker game, the monetary component is at the centre; players place bids and exchange money at every step of the game. Furthermore, it is comprised of a fixed set of players who have conflicting goals (i.e. win the game), don't fully trust each other but are still willing to collaborate given the agreed set of rules."),(0,n.kt)("div",{className:"admonition admonition-tip alert alert--success"},(0,n.kt)("div",{parentName:"div",className:"admonition-heading"},(0,n.kt)("h5",{parentName:"div"},(0,n.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,n.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"},(0,n.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M6.5 0C3.48 0 1 2.19 1 5c0 .92.55 2.25 1 3 1.34 2.25 1.78 2.78 2 4v1h5v-1c.22-1.22.66-1.75 2-4 .45-.75 1-2.08 1-3 0-2.81-2.48-5-5.5-5zm3.64 7.48c-.25.44-.47.8-.67 1.11-.86 1.41-1.25 2.06-1.45 3.23-.02.05-.02.11-.02.17H5c0-.06 0-.13-.02-.17-.2-1.17-.59-1.83-1.45-3.23-.2-.31-.42-.67-.67-1.11C2.44 6.78 2 5.65 2 5c0-2.2 2.02-4 4.5-4 1.22 0 2.36.42 3.22 1.19C10.55 2.94 11 3.94 11 5c0 .66-.44 1.78-.86 2.48zM4 14h5c-.23 1.14-1.3 2-2.5 2s-2.27-.86-2.5-2z"}))),"Decentralized randomness & Multi-Party Computation")),(0,n.kt)("div",{parentName:"div",className:"admonition-content"},(0,n.kt)("p",{parentName:"div"},"For this use case, we assume that there's a way an agreed-upon way to implement a decentralized poker game with pseudo-randomness or multi-party computation (see, for example ",(0,n.kt)("a",{parentName:"p",href:"https://eprint.iacr.org/2018/157"},"ROYALE by David & al"),"). We focus on the state channel aspect of the story for which Hydra heads provide a solution. "))),(0,n.kt)("p",null,"In a poker game, every player can embody a Hydra Head member, running its own Hydra node. Each participant starts a Head by committing funds to that Head. These represent their chips. Once the Head is established, participants can begin playing the game by leveraging on-head Plutus contracts. Players can instantly process the transfer of funds inside the Head. This is a simple scenario where participants mainly send money to one another, mediated by a script (acting as the game dealer, warrants of the rules and good progress of the game). "),(0,n.kt)("p",null,(0,n.kt)("img",{loading:"lazy",src:a(355).Z,width:"1043",height:"601"})),(0,n.kt)("p",null,"Eventually, the game reaches an end with a well-defined distribution of funds. Participants can then play another game or close the Head and write the end result onto the Layer 1. The whole game(s) is (are) unknown of the Layer 1. Only the final UTxO distribution is. "),(0,n.kt)("p",null,"Since the game only involves basic payments and script interactions, it could have been played entirely on the Layer 1, without reaching for a Hydra head. However, a Hydra head provides fast-paced transactions for the course of the game and cheap (or even none) fees -- besides the costs needed to establish the Hydra Head."))}d.isMDXComponent=!0},355:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/poker-5da0a259ae448b547c2059af88b66f2b.webp"}}]);