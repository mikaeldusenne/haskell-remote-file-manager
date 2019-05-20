var i = 1;

function toggle_visibility_upload(){
	$( '#upload-progress' ).toggle();
	$( '#div-input-file' ).toggle();
}

function newfolder_show(){
	$( '#li-new-folder' ).show();
}

function createFolder(){
	var fieldVal = $("#new-folder-field").val();
	console.log(fieldVal);
	
}

function updateFileUploadPath(){
	var fieldVal = $("#file-upload-field").val();
	
	// Change the node's value by removing the fake path (Chrome)
	fieldVal = fieldVal.replace("C:\\fakepath\\", "");
	
	if (fieldVal != undefined || fieldVal != "") {
		$(".custom-file-label").html(fieldVal);
	}
}

$("#file-upload-field").change(updateFileUploadPath);

function init(){
	updateFileUploadPath();
}


(function( $ ) {
	$( '#upload-progress' ).hide();

    var reader = {};
    var file = {};
    var slice_size = 1000 * 1024;

	
    function start_upload( event ) {
        event.preventDefault();

        reader = new FileReader();
        file = document.querySelector( '#file-upload-field' ).files[0];
		console.log("starting the upload of ");
		console.log(file);

        upload_file( 0 );
    }
    $( '#file-upload-submit' ).on( 'click', start_upload );


	var upload_chunk = function( event ) {
			console.log("onloadevent");
			d = {
				action: 'dbi_upload_file',
				file_data: event.target.result,
				file: file.name,
				file_type: file.type,
				nonce: token
			};
			console.log(d);
			if ( event.target.readyState !== FileReader.DONE ) {
				return;
			}
			$.ajax( {
				url: 'fileupload',
				type: 'POST',
				dataType: 'json',
				// contentType: "application/json; charset=utf-8",
				// contentType: "x-www-form-urlencoded",
				cache: false,
				data: d,
				// beforeSend: function(x) {
				// 	if (x && x.overrideMimeType) {
				// 		x.overrideMimeType("application/json;charset=UTF-8");
				// 	}
				// },
				error: function( jqXHR, textStatus, errorThrown ) {
					console.log("ajax upload error");
					console.log( jqXHR, textStatus, errorThrown );
					$( '#upload-progress' ).html( 'Error Uploading File' );
				},
				success: function( data ) {
					var size_done = start + slice_size;
					var percent_done = Math.floor( ( size_done / file.size ) * 100 );

					if ( next_slice < file.size ) {
						// Update upload progress
						$( '#upload-progress' ).html( 'Uploading File - ' + percent_done + '%' );

						// More to upload, call function recursively
						upload_file( next_slice );
					} else {
						// Update upload progress
						$( '#upload-progress' ).html( 'Upload Complete!<button onclick="toggle_visibility_upload()" class="btn btn-light btn-sm mx-2">ok</button>' );
					}
				}
			} );
	};
	
    function upload_file( start ) {
		if(start === 0){
			toggle_visibility_upload();
		}
		var next_slice = start + slice_size + 1;
		var blob = file.slice( start, next_slice );
		reader.onloadend = upload_chunk;
		reader.readAsDataURL( blob );
    }

})( jQuery );
// 
