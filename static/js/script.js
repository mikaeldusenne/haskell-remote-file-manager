(function(window,$){
	Vue.use(VueResource);
	// Vue.use(VueRouter);
	
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
		refreshing: true

	};
	
	// check if a "contenttype" is in the "response"'s headers
	var contentTypeIs = (response, contenttype) =>
		response.headers.get('content-type').indexOf(contenttype) != -1;

	var identity = e => e;


	function mkfullpath(path){
		if( path === ".." ){
			fullpath = self.data.currentPath.split('/');
			fullpath.pop(-1);
			fullpath = fullpath.join('/');
		}else {
			fullpath = [self.data.currentPath, path].filter(e => e.length).join('/');
		}
		return fullpath;
	}
	
	this.update = function(path=""){
		console.log("update");
		self.data.refreshing = true;
		// self.reset_data_values();
		var fullpath = mkfullpath(path);
		

		// console.log(fullpath);
		
		Vue.http.get('/api/all/', {params: {path: fullpath}})
			.then(response => {
				var o = JSON.parse(response.bodyText);
				console.log(o);
				// Object.keys(o).forEach(k => self.data[k] = o[k])
				self.data.filelist = o.files;
				self.data.deviceSpaceInfo = o.space;
				self.data.currentPath = o.datapath;
				self.data.refreshing = false;

			}, response => {
				console.log("error while updating data");
				console.log(response);
			});
		
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
	
	function start_upload() {
		event.preventDefault();

		console.log("starting the upload of ");
		console.log(self);
		
		Vue.set(self.data, 'show_progress_upload', true);
		
		var reader = new FileReader();
		var slice_size = 1000 * 1024 ;

		var file = self.data.file_to_upload;

		// $( '#file-upload-submit' ).on( 'click', start_upload );
		var f_upload_progress = function( data ) {
			console.log("progress");
			console.log(data);
			// var size_done = start + slice_size;
			// var percent_done = Math.floor( ( size_done / file.size ) * 100 );

			// if ( next_slice < file.size ) {
			// 	$( '#upload-progress' ).html( 'Uploading File - ' + percent_done + '%' );
			// 	upload_file( next_slice );
			// } else {
			// 	$( '#upload-progress' ).html( 'Upload Complete!<button onclick="toggle_visibility_upload()" class="btn btn-light btn-sm mx-2">ok</button>' );
			// }
		};
		
		toggle_visibility_upload();
		var upload_chunk = function(start, event){
			if ( event.target.readyState !== FileReader.DONE ) return; // ????

			d = {
				file_data: event.target.result,
				file: file.name,
				file_type: file.type,
				first_chunk: (start - slice_size - 1 === 0)
			};
			// d = file;
			console.log(d);
			Vue.http.post('fileupload',
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


	
	this.methods = {
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
		addpathspaces: function(p){
			return p.split('/').join(' / ');
		},
		fileAction: function(file){
			if(file.filetype.type==='Dir'){
				update(file.path);
			}else{
				window.open(
					"?path=" + mkfullpath(file.path),
					'_blank'
				);
			}
		},
		mkfullpath: mkfullpath,
		set_upload_file: set_upload_file,
		start_upload: start_upload
		
	};
	
	const app = new Vue({
		el: '#app',
		data: self.data,
		methods: self.methods

		// router
	});
	
	// debug
	window.data = data;

	this.update();

})(window,jQuery);

