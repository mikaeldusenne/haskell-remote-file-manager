(function(window,$){
	Vue.use(VueResource);
	Vue.use(VueRouter);
	
	// const store = new Vuex.store({
	// 	state:{
			
	// 	}
	// });
	$.ajaxSetup({
		type: "POST",
		data: {},
		dataType: 'json',
		xhrFields: {
			withCredentials: true
		},
		crossDomain: true,
		contentType: 'application/json; charset=utf-8'
	});
	
	var self = this;


	self.stripPath = path => path.replace(/^\//, "").replace(/\/$/, "");

	
	this.same_string = (sa, sb) => {
		var f = s => s.replace(/ /g,"");
		return f(sa) == f(sb);
	};
	this.pretty = s => JSON.stringify(s, null, 2);
	
	this.data = {
		filelist: [],
		currentPath: "/",
		deviceSpaceInfo: {avail:0, used: 0},

		file_to_upload: null,
		upload_status: '',
		show_progress_upload: false,
		refreshing: true,

		show_new_folder: false,
		new_folder_name: "",
		selectedItem: null,

	};
	
	// check if a "contenttype" is in the "response"'s headers
	var contentTypeIs = (response, contenttype) =>
		response.headers.get('content-type').indexOf(contenttype) != -1;

	var identity = e => e;


	function mkfullpath(path){
		if(path === undefined){
			return("");
		}else if( path === ".." ){
			fullpath = self.data.currentPath.split('/');
			fullpath.pop(-1);
			fullpath = fullpath.join('/');
		}else {
			if(isfull(path)){
				fullpath = path;
			}else{
				fullpath = [self.data.currentPath, path].filter(e => e.length).join('/');
			}
		}
		console.log("mkfullpath  " + fullpath);
		return fullpath;
	}

	var isfull = function(p){
		var f = e => e.split('/')[0];
		return p==="" || f(self.data.currentPath) == f(p);
	};
	
	this.update_status = function(path=undefined){
		console.log("status called with:" + path);
		if(path===undefined) path = self.data.currentPath;
		console.log("pathhhh status : " + path);
		var fullpath = mkfullpath(path);
		console.log("so the fullpath is : " + fullpath);
		
		return getWithPath('/api/status/', fullpath)
			.then(response => {
				var o = JSON.parse(response.bodyText);
				self.data.deviceSpaceInfo = o.space;
				Vue.set(self.data, "currentPath", o.datapath);
				Vue.set(self.data, 'selectedItem', undefined);
				console.log("Update status: setting current path to: " + o.datapath);
			}, response => {
				console.log("error while updating data");
				console.log(response);
			});
	};
	
	this.update = function(path=undefined){
		if(path===undefined) path = self.data.currentPath;
		console.log('update ===> ' + path);
		var fullpathorig = mkfullpath(path);
		console.log('updateee ORIG -=-=->' + fullpathorig);
		
		self.update_status(path=fullpathorig)
			.then( function(){
				console.log("pathhhh updt : " + path);
				console.log("update after status");
				self.data.refreshing = true;
				// self.reset_data_values();
				console.log("path ORIG from update filedetail after status update: " + fullpathorig);
				
				
				getWithPath('/api/files/', fullpathorig).then(response => {
						var o = JSON.parse(response.bodyText);
						console.log(o);
						Vue.set(self.data, "filelist", o.files);
						self.data.refreshing = false;
						console.log(fullpathorig);
						
					}, response => {
						console.log("error while updating data");
						console.log(response);
					});
			});
	};


	var getWithPath = (url, path) => Vue.http.get(url, {params:{
		path: self.stripPath(path)}});
	
	function fileDownload(file){

		window.open(
			"/api/download/?path=" + stripPath(mkfullpath(file.path)),
			'_blank'
		);

		// getWithPath('/api/download', file.path).then(
		// 	function(success){
		// 		console.log("Download of " + file.path + "ok");
		// 	},
		// 	function(error){
		// 		console.log("Error with download of " + file.path + "!");
		// 		console.log(error);
		// 	}
		// );
	};

	

	// // Routing
	// const routes = [
	// 	{ path: '/',
	// 	  component: Vue.component('request-tester'),
	// 	  alias: '/main'
	// 	},
	// 	{ path: '/about', component: Vue.component('about') },
	// 	{ path: '/table', component: Vue.component('vue-table') },
	// 	{ path: '/mapping-editor', component: Vue.component('mapping-editor') }
	// ];
	// // Router options
	// const router = new VueRouter({
	// 	linkActiveClass: 'active',
	// 	routes
	// })
	// router.afterEach((to, from) => {
	// 	console.log("hash change: " + from.path + " -> " + to.path);
	// 	self.update()
	// })

	function set_upload_file(event){
		Vue.set(self.data, 'file_to_upload', event.target.files[0]);
	}

	function dirname(a){
		a = a.split('/');
		a.pop();
		return( a.join('/') );
	}

	var basename = function(a){
		// console.log(a);
		// return a.split('/').pop();
		if(a === ""){
			return "";
		}else{
			return a.split('/').pop();
		}

	};
	
	var prettybasename = function(a){
		// console.log('*****************************************');
		// console.log(a);
		if(a === "" || a === "/"){
			return "home";
		}else{
			return a.split('/').pop();
		}
	};
	
	
	
	function start_upload() {
		event.preventDefault();
		
		var file = self.data.file_to_upload;		
		if(file === null) return;

		console.log("starting the upload of:");
		console.log(file);
		
		Vue.set(self.data, 'show_progress_upload', true);
		
		var reader = new FileReader();
		var slice_size = 1000 * 1024 ;


		// $( '#file-upload-submit' ).on( 'click', start_upload );
		var f_upload_progress = function( data ) {
			console.log("progress");
			console.log(data);
		};
		
		toggle_visibility_upload();
		var upload_chunk = function(start, event){
			if ( event.target.readyState !== FileReader.DONE ) return; // ????

			d = {
				file_data: event.target.result,
				file:  mkfullpath(file.name),
				file_type: file.type,
				path: self.currentPath,
				first_chunk: (start - slice_size - 1 === 0) // for append mode or not
			};
			console.log(d);
			Vue.http.post('api/fileupload',
						  body=d,
						  timeout=0,
						  emulateJSON=false,
						  uploadProgress=self.f_upload_progress).
				then(function(callbacksuccess){
					console.log(callbacksuccess);
					console.log("ok");
					var size_done = start + slice_size;
					var percent_done = Math.min(
						Math.floor( ( size_done / file.size ) * 100 ),
						100);
					console.log(percent_done);
					Vue.set(self.data, 'upload_status', percent_done + '% uploaded');
					upld(start);
				}, function(error){
					console.log("ajax upload error");
					console.log(error);
					upld(start - slice_size - 1);
					Vue.set(self.data, 'upload_status', 'Error Uploading File');
				});
		};

		var upld = function(start){
			if(start < file.size){
				console.log(start);
				var next_slice = start + slice_size + 1;
				var blob = file.slice( start, next_slice );
				reader.onloadend = e => upload_chunk(next_slice, e);
				reader.readAsDataURL( blob );
			}else{
				Vue.set(self.data, 'show_progress_upload', false);
				self.update();
			}
		};
		upld(0);
	};

	function cd(path){
		update(path);
	}
	
	function fileAction(file){
		if(file.filetype.type==='Dir'){
			update(file.path);
		}else{
			window.open(
				"?path=" + mkfullpath(file.path),
				'_blank'
			);
		}
	};
	
	this.methods = {
		isFile: e => e.filetype.type === 'File',
		isDir: e => e.filetype.type === 'Dir',
		isSelected: file => file.path == self.data.selectedItem,
		selectItem: function(which){
			console.log("select " + which);
			already = self.data.selectedItem;
			next = which === already ? undefined : which;
			
			Vue.set(self.data, 'selectedItem', next);
		},
		prettyBytes: function(b){
			var byte_units = ["kB","MB","GB","TB"];
			function f(k, l){
				if(l.length==1){
					if(Math.round(k) != k) k = k.toFixed(2);
					return (k + " " + l);
				}else{
					if(k<1024) return(f(k, [l[0]]));
					else return( f(k/1024, l.slice(1)) );
				}
			}
			return(f(b, byte_units));
		},
		update: self.update,
		cd: cd,
		update_status: self.update_status,
		addpathspaces: function(p){
			return p.split('/').join(' / ');
		},
		fileAction: fileAction,
		fileDownload: fileDownload,
		previousPath: function(){
			cd('..');
		},
		createFolder: function(){
			name = mkfullpath( self.data.new_folder_name );
			Vue.http.post('api/newfolder', {folderpath: name})
				.then(
					function(success){
						console.log("folder created");
						self.update();
						Vue.set(self.data, 'show_new_folder', false);
						Vue.set(self.data, 'new_folder_name', "");
					},
					function(error){
						console.log("error creating folder");
						console.log(error);
					}).then(self.update());
		},
		mkfullpath: mkfullpath,
		set_upload_file: set_upload_file,
		start_upload: start_upload,
		dirname: dirname,
		basename: basename,
		prettybasename: prettybasename,
		
	};

	var router = new VueRouter({
		mode: 'history',
		routes: [],

	});

	// router.afterEach((to, from) => {
	// 	console.log("hash change: " + from.path + " -> " + to.path);
	// 	self.update(to.path);
	// });
	
	const app = new Vue({
		router,
		el: '#app',
		data: self.data,
		computed: {
			currentpathBar: function(){
				console.log('current path from BAR ----------:' + self.data.currentPath);
				p = self.data.currentPath.split('/');
				console.log(p);
				console.log([...Array(p.length).keys()].map(n => p.slice(0,n+1)));
				ans = ( [...Array(p.length).keys()]
						.map(n => p.slice(0,n+1))
						.map(a => a.join('/')));
				console.log("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  " + ans);
				if(ans[0].length>0){
					console.log('PREPEND');
					ans.unshift("");
					console.log('ppp');
					console.log(ans);
				}
				console.log(ans);
				return ans;
				
			}
		},
		methods: self.methods,
		created: function() {
			path = this.$route.query.path;
			if(path !== undefined){
				console.log("ROUTE path:");
				self.data.currentPath = path;
			}
		},
		watch: {
			'$route' (to, from) {
				console.log("hash change---> " + from.path + " -> " + to.path);
				self.update(to.path);
			}
		}
		// router
	});
	
	// debug
	window.data = data;

	// console.log(router.currentRoute);
//	setTimeout(() => {
		p = router.currentRoute.path;
		console.log(p);
		self.data.currentPath = p;
		this.update(p);
//	}, 1000);

})(window,jQuery);

