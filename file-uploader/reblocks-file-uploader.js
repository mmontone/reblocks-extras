function fileUploaderUpload(id) {
  const fileInput = document.getElementById(id);
  const progressBar = document.getElementById(id + '-progress');
  const status = document.getElementById(id + '-status');
  const file = fileInput.files[0];
  const formData = new FormData();
  formData.append(id, file);

  const xhr = new XMLHttpRequest();
  xhr.open("POST", "/upload/file/" + id); 

  xhr.upload.addEventListener("progress", function(evt) {
    if (evt.lengthComputable) {
      const percentComplete = (evt.loaded / evt.total) * 100;
      progressBar.value = percentComplete;
      status.textContent = `${Math.round(percentComplete)}%`;
    }
  });

  xhr.onload = function() {
    if (xhr.status === 200) {
      status.textContent = "Upload successful!";
    } else {
      status.textContent = "Upload failed.";
    }
  };

  xhr.onerror = function() {
    status.textContent = "Upload failed.";
  };

  xhr.send(formData);
}
