package net.p3pp3rf1y.sophisticatedbackpacks.util;

public class WeightedElement<T> {
	private final int weight;
	private final T element;

	public WeightedElement(int weight, T element) {
		this.weight = weight;
		this.element = element;
	}

	public int getWeight() {
		return weight;
	}

	public T getElement() {
		return element;
	}
}
