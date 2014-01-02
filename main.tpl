<h1>{{ hello }}</h1>
<p>{% include test.tpl %}</p>
{% for toto in patate %}
	<h2>{{ toto.lorem }}</h2>
	<ul>
	{% for orange in toto.frite %}
		<li>{{ orange.tr }}</li>
	{% endfor %}
	</ul>
{% endfor %}