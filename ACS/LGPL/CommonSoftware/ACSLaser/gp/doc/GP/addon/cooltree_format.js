var COOLTREE_FORMAT =
[
//0. left position
	5,
//1. top position
	120,
//2. show +/- buttons
	true,
//3. couple of button images (collapsed/expanded/blank)
	["images/menu_pil.gif", "images/menu_pil_active.gif", "images/b.gif"],
//4. size of images (width, height,ident for nodes w/o children)
	[14,14,0],
//5. show folder image
	false,
//6. folder images (closed/opened/document)
	["images/b.gif", "images/b.gif", "images/b.gif"],
//7. size of images (width, height)
	[8,2],
//8. identation for each level [0/*first level*/, 16/*second*/, 32/*third*/,...]
	[0,15,30,45],
//9. tree background color ("" - transparent)
	"",
//10. default style for all nodes
	"clsDemoNode",
//11. styles for each level of menu (default style will be used for undefined levels)
	["clsDemoNode","clsDemoNode1","clsDemoNode1","clsDemoNode1","clsDemoNode1"],
//12. true if only one branch can be opened at same time
	true,
//13. item pagging and spacing
	[0,2],
];
