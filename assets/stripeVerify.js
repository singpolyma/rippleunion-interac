window.addEventListener('load', function(){
	if(window.location.hash != '#stripeVerify') {
		// Hack so it doesn't fade out on load
		document.getElementById('stripeVerify').style.display = 'none';
		setTimeout(function(){
			document.getElementById('stripeVerify').style.display = 'block';
		}, 200);
	} else {
		document.getElementById('stripeVerify').style.visibility = 'visible';
		document.getElementById('stripeVerify').style.opacity = 1;
	}

	var a = document.createElement('a');
	a.href = '#';
	a.appendChild(document.createTextNode('Cancel'));
	document.getElementById('stripeVerify').appendChild(a);

	document.getElementById('stripeVerify').className += ' modal';
}, false);
window.addEventListener('hashchange', function() {
	if(window.location.hash == '#stripeVerify') {
		document.getElementById('stripeVerify').style.visibility = 'visible';
		document.getElementById('stripeVerify').style.opacity = 1;
	} else {
		document.getElementById('stripeVerify').style.visibility = 'hidden';
		document.getElementById('stripeVerify').style.opacity = 0;
	}
}, false);
