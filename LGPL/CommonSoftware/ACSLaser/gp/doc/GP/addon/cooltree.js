// Title: COOLjsTree
// Description: See the demo at url
// URL: http://javascript.cooldev.com/scripts/cooltree/
// Version: 1.1.8
// Date: 02-18-2002 (mm-dd-yyyy)
// Last Modify: 04-17-2002
// Author: Sergey Nosenko <jsinfo@cooldev.com>
// Notes: Registration needed to use this script on your web site.
// Registration for this version is FREE for evaluating and personal use.
// See official site for details
// Copyright (c) 2001-2002 by CoolDev.Com
// Copyright (c) 2001-2002 by Sergey Nosenko
var NTrees = [];
function bw_check(){var is_major = parseInt(navigator.appVersion);this.ver=navigator.appVersion;this.agent=navigator.userAgent;this.dom=document.getElementById?1:0;this.opera=this.agent.indexOf("Opera")>-1;this.ie5=(this.ver.indexOf("MSIE 5")>-1 && this.dom && !this.opera)?1:0;this.ie6=(this.ver.indexOf("MSIE 6")>-1 && this.dom && !this.opera)?1:0;this.ie4=(document.all && !this.dom && !this.opera)?1:0;this.ie=this.ie4||this.ie5||this.ie6;this.mac=this.agent.indexOf("Mac")>-1;this.ns6=(this.dom && parseInt(this.ver) >= 5) ?1:0;this.ie3 = (this.ver.indexOf("MSIE") && (is_major < 4));this.hotjava = (this.agent.toLowerCase().indexOf('hotjava') != -1)? 1:0;this.ns4=(document.layers && !this.dom && !this.hotjava)?1:0;this.bw=(this.ie6 || this.ie5 || this.ie4 || this.ns4 || this.ns6 || this.opera);this.ver3 = (this.hotjava || this.ie3);return this;}
function CTreeFormat( fmt, tree ){
	this.init = function( fmt, tree ){this.left=fmt[0];this.top=fmt[1];this.showB  = fmt[2];this.clB  = fmt[3][0];this.exB  = fmt[3][1];this.iE  = fmt[3][2];this.Bw  = fmt[4][0];this.Bh  = fmt[4][1];this.Ew  = fmt[4][2];this.showF  = fmt[5];this.clF  = fmt[6][0];this.exF  = fmt[6][1];this.iF  = fmt[6][2];this.Fw  = fmt[7][0];this.Fh  = fmt[7][1];this.ident = fmt[8];this.back = new CTreeBack(this.left, this.top, fmt[9], 'cls'+tree.name+'_back');this.nst = fmt[10];this.nstl = fmt[11];this.so = fmt[12];this.pg = fmt[13][0];this.sp = fmt[13][1];if (this.showB){this.e = new Image();this.e.src = this.clB;this.e1 = new Image();this.e1.src = this.exB;this.e5 = new Image();this.e5.src = this.iE;}if (this.showF){this.e2 = new Image();this.e2.src = this.exF;this.e3 = new Image();this.e3.src = this.clF;this.e4 = new Image();this.e4.src = this.iF;}}
	this.nstyle = function ( lvl ){return ( und(this.nstl[lvl]) ) ? this.nst : this.nstl[lvl];}
	this.idn = function( lvl ){var r = ( und(this.ident[lvl]) ) ? this.ident[0]*lvl : this.ident[lvl];return r;}
	this.init(fmt, tree);
}
function COOLjsTree( name, nodes, format )
{
	this.REGISTERED = false;
	this.bw = new bw_check();
	this.ns4 = this.bw.ns4;
	this.name = name;
	this.fmt = new CTreeFormat(format, this);
	if (typeof(NTrees) == 'undefined') NTrees = new Array();
	NTrees[this.name] = this;
	this.Nodes = new Array();
	this.rootNode = new CTreeNode(null, "", "", "", null);
	this.rootNode.treeView = this;
	this.selectedNode = null;
	this.maxWidth = 0;
	this.maxHeight = 0;
    this.ondraw = null;	
	this.nbn = function( nm ){
		for (var i = 0;i<this.Nodes.length;i++)
			if (this.Nodes[i].text == nm)
				return this.Nodes[i];
		return null;
	}
	this.addNode = function (node)
	{
		var parentNode = node.parentNode;
		this.Nodes = this.Nodes.concat([node]);
		node.index = this.Nodes.length - 1;
		if (parentNode == null) {
			this.rootNode.children = this.rootNode.children.concat([node]);
		}
		else
			parentNode.children = parentNode.children.concat([node]);
		return node;
	}
	this.rebuildTree = function()
	{
		var s = "";
		for (var i = 0; i < this.Nodes.length; i++){
			s += this.Nodes[i].init();
		}
		document.write(s);
		for (var i = 0; i < this.Nodes.length; i++)
		if (this.ns4) {
			this.Nodes[i].el = document.layers[this.Nodes[i].id()+"d"];
			if (this.fmt.showF)this.Nodes[i].nf = this.Nodes[i].el.document.images[this.Nodes[i].id()+"nf"];
			if (this.fmt.showB)this.Nodes[i].nb = this.Nodes[i].el.document.images[this.Nodes[i].id()+"nb"];
		} else {
			this.Nodes[i].el = document.all? document.all[this.Nodes[i].id()+"d"] : document.getElementById(this.Nodes[i].id()+"d");		
			if (this.fmt.showB)this.Nodes[i].nb = document.all? document.all[this.Nodes[i].id()+"nb"] : document.getElementById(this.Nodes[i].id()+"nb");		
			if (this.fmt.showF)this.Nodes[i].nf = document.all? document.all[this.Nodes[i].id()+"nf"] : document.getElementById(this.Nodes[i].id()+"nf");		
		}
	}
	this.draw = function()
	{
		this.currTop = this.fmt.top;
		this.maxHeight =0; this.maxWidth=0;
		for (var i = 0; i < this.rootNode.children.length; i++)
			this.rootNode.children[i].draw(true);
		this.fmt.back.resize(this.maxWidth-this.fmt.left, this.maxHeight - this.fmt.top);
		if (this.ondraw != null) this.ondraw();
	}
	
	this.updateImages = function ( node )
	{
		var srcB = node.expanded? this.fmt.exB : this.fmt.clB;
		var srcF = node.expanded? this.fmt.exF : this.fmt.clF;
		if (node.treeView.fmt.showB && node.nb && node.nb.src != srcB) node.nb.src = srcB;
		if (node.treeView.fmt.showF && node.nf && node.nf.src != srcF) node.nf.src = node.hasChildren() ? srcF : this.fmt.iF;
	}
	this.expandNode = function( index )
	{
		var node = this.Nodes[index];
		var pNode = node.parentNode ? node.parentNode : null;
		if (!und(node) && node.hasChildren())
		{
			node.expanded = !node.expanded;
			this.updateImages(node);
			if (!node.expanded){
				node.hideChildren();
			} else {
				if (this.fmt.so)// && node.parentNode == null)
				{
					for (var i = 0; i < this.Nodes.length; i++){
						this.Nodes[i].show(false);
						if ( this.Nodes[i] != node && this.Nodes[i].parentNode == pNode) {
							this.Nodes[i].expanded = false;
							this.updateImages(this.Nodes[i]);
						}
					}
				}
			}
            this.draw();
		}
	}

	this.selectNode = function( index )
	{
		var node = this.Nodes[index];
		if ( !und(node) )
			this.selectedNode = node;
		node.draw();
	}
	
	this.readNodes = function (nodes)
	{
		var ind = 0;
		var par = null;
		function readOne( arr , tree)
		{
			if (und(arr)) return;
			var text = arr[0];
			var url = arr[1] == null? "": arr[1];
			var targ = arr[2] == null? "": arr[2];
			var node = tree.addNode(new CTreeNode(tree, par, text, url, targ))
			var i = 3;
			while (!und(arr[i]))
			{
				par = node;
				readOne(arr[i], tree);
				i++;
			}
		}
		if (und(nodes) || und(nodes[0]) || und(nodes[0][0])) return;
		for (var i = 0; i < nodes.length; i++){
			par = null;
			readOne(nodes[i], this);
		}
	}
	this.collapseAll = function( rd )
	{
		for (var i = 0; i < this.Nodes.length; i++){
			if (this.Nodes[i].parentNode != this.rootNode)
				this.Nodes[i].show(false);
			this.Nodes[i].expanded = false;
			this.updateImages(this.Nodes[i]);
		}
		if (rd) this.draw();
	}
	this.expandAll = function( rd )
	{
		for (var i = 0; i < this.Nodes.length; i++){
			this.Nodes[i].expanded = true;
			this.updateImages(this.Nodes[i]);
		}
		if (rd) this.draw();
	}
	this.init = function()
	{
		this.readNodes(nodes);
		this.rebuildTree();
		this.draw();
	}
	this.init();
}

function CTreeNode( treeView, parentNode , text, url, target){
	this.index = -1;
	this.treeView = treeView;
	this.parentNode = parentNode;
	this.text = text;
	this.url = url;
	this.target = target;
	this.expanded = false;
	this.children = new Array();
	this.level = function(){
		var node = this;
		var i = 0;
		while (node.parentNode != null){
			i++;
			node = node.parentNode;
		}
		return i;
	}
	this.hasChildren = function(){
		return this.children.length > 0;
	}
	this.init = function(){
		var s = "";
		if (this.treeView.ns4) {
			s = '<layer id="'+this.id()+'d" z-index="'+this.index+10+'" visibility="hidden">'+this.getContent()+'</layer>';
		} else {
			s = '<div id="'+this.id()+'d" style="position:absolute;visibility:hidden;z-index:'+this.index+10+';">'+this.getContent()+'</div>';
		}
		return s;
	}
	this.getH = function(){return this.treeView.ns4 ? this.el.clip.height : this.el.offsetHeight;}
    this.getW = function(){return this.treeView.ns4 ? this.el.clip.width : this.el.offsetWidth;}
	this.id = function(){return 'nt'+this.treeView.name+this.index;}
	this.getContent = function(){
		function itemSquare(node){
				var img = node.hasChildren() ? (node.expanded ? node.treeView.fmt.exF : node.treeView.fmt.clF) : node.treeView.fmt.iF;
				var w = node.treeView.fmt.Fw; var h = node.treeView.fmt.Fh;
				return "<td valign=\"middle\" width=\""+w+"\"><img id=\""+node.id()+"nf\" name=\""+node.id()+"nf\" src=\"" + img + "\" width="+w+" height="+h+" border=0></td>\n";
		}
		function buttonSquare(node){
			
			var img = node.expanded ? node.treeView.fmt.exB : node.treeView.fmt.clB;
			var w = node.treeView.fmt.Bw; var h = node.treeView.fmt.Bh;
			return '<td valign=\"middle\" width="'+w+'"><a href="javascript:NTrees[\''+node.treeView.name+'\'].expandNode('+node.index+')"><img name=\''+node.id()+'nb\' id=\''+node.id()+'nb\' src="' + img + '" width="'+w+'" height="'+h+'" border=0></a></td>\n';
		}
		function blankSquare(node, ww){
			var img = node.treeView.fmt.iE;
			return "<td width=\""+ww+"\"><img src=\"" + img + "\" width="+ww+" height=1 border=0></td>\n"
		}
	
		var s = '';
		var ll = this.level();
		s += '<table cellpadding='+this.treeView.fmt.pg+' cellspacing='+this.treeView.fmt.sp+' border=0 class="cls'+this.treeView.name+'_back'+ll+'"><tr>';
		//ident
		var idn = this.treeView.fmt.idn(ll);
		if (idn > 0)
			s += blankSquare(this, idn);
		if ( this.treeView.fmt.showB)
			s += this.hasChildren() ? buttonSquare(this) : blankSquare(this, this.treeView.fmt.Ew);
		if ( this.treeView.fmt.showF)
			s += itemSquare(this);
		if ( this.url == "")
		{	
			s += this.hasChildren()? '<td nowrap=\"1\"><a class="'+this.treeView.fmt.nstyle(ll)+'" href="javascript:NTrees[\''+this.treeView.name+'\'].expandNode('+this.index+')">'+this.text+'</a></td></tr></table>' : '<td nowrap=\"1\"><a class="'+this.treeView.fmt.nstyle(ll)+'" href="#">'+this.text+'</a></td></tr></table>';
		} else {
			s += '<td nowrap=\"1\"><a class="'+this.treeView.fmt.nstyle(ll)+'" href="'+this.url+'" target="'+this.target+'" onclick="javascript:NTrees[\''+this.treeView.name+'\'].expandNode('+this.index+')">'+this.text+'</a></td></tr></table>';
		}
		return s;
	}
	this.moveTo = function( x, y ){if (this.treeView.ns4)this.el.moveTo(x,y);else{this.el.style.left=x;this.el.style.top=y;}}
	this.show = function(sh){if (this.visible == sh)return;this.visible = sh;var vis = this.treeView.ns4 ? (sh ? 'show': 'hide') : (sh ? 'visible': 'hidden');if (this.treeView.ns4)this.el.visibility=vis;else this.el.style.visibility = vis;}
	this.hideChildren = function(){this.show(false);for (var i = 0; i < this.children.length; i++)this.children[i].hideChildren();}
	this.draw = function(){var ll = this.treeView.fmt.left;this.moveTo(this.treeView.fmt.left, this.treeView.currTop);if (ll+this.getW() > this.treeView.maxWidth)this.treeView.maxWidth = ll+this.getW();this.show(true);this.treeView.currTop += this.getH();if (this.treeView.currTop > this.treeView.maxHeight)this.treeView.maxHeight = this.treeView.currTop;if (this.expanded && this.hasChildren() )for (var i = 0; i < this.children.length; i++)this.children[i].draw();}
}
function CTreeBack( aleft, atop, color, name ){
	this.bw = new bw_check();this.ns4 = this.bw.ns4;this.left = aleft;this.top = atop;this.name = name;this.color = color;this.t = unescape('%4E%6F%73%54%72%65%65%20%26%63%6F%70%79%3B%20%43%6F%6F');
	this.resize = function(w,h){if (this.ns4){this.el.resizeTo(w,h);}else{this.el.style.width=w;this.el.style.height=h;if (this.r) this.el2.style.top=h+this.top-5;}};this.t = this.t + unescape('%6C%44%65%76%2E%43%6F%6D');
	this.init = function(){if (this.r)if (!this.ns4) {var bgc = this.color == ""? "" : " background-color:"+this.color+";";document.write('<div id="'+this.name+'c" style="'+bgc+'position:absolute;z-index:0;top:'+this.top+'px;left:'+this.left+'px">'+'&nbsp;<span style="font-size:7px;color:#d0d0d0;text-decoration:none;">'+this.t+'</span>'+'</div>');this.el2 = document.all? document.all[this.name+'c'] : document.getElementById(this.name+'c');}if(this.ns4){var bgc = this.color == ""? "" : ' bgcolor="'+this.color+'" ';document.write('<layer '+bgc+' top="'+this.top+'" left="'+this.left+'" id="'+this.name+'" z-index="0">'+ '</layer>');this.el = document.layers[this.name];} else {var bgc = this.color == ""? "" : " background-color:"+this.color+";";document.write('<div id="'+this.name+'" style="'+bgc+'position:absolute;z-index:0;top:'+this.top+'px;left:'+this.left+'px">'+ '</div>');this.el = document.all? document.all[this.name] : document.getElementById(this.name);}};this.r = true;
	this.init();
}
function und( val ){return typeof(val) == 'undefined';}
window.oldCTOnLoad = window.onload;
window.onload = function ()
{
	var bw = new bw_check();
	if (bw.ns4 || bw.opera)
	{
		window.origWidth  = this.innerWidth;
		window.origHeight = this.innerHeight;
		if (bw.opera && und(window.operaRH)) {
			window.operaRH = 1;
			resizeHandler();
		}
	}
	if (window.oldCTOnLoad) window.oldCTOnLoad();
}
function resizeHandler() {
	var bw = new bw_check();
    if (this.innerWidth != window.origWidth || this.innerHeight != window.origHeight) location.reload();
    if (bw.opera) {
		setTimeout('resizeHandler()',500);
	}
}
if (new bw_check().ns4) window.onresize = resizeHandler;
