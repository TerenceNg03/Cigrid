int main() {
	int x = 0;
	int i = 0;
	{
		i = 0;
		while (i < 10) {
			x = x + 2;
			i++;
		}
	}
	return x - 20;
}