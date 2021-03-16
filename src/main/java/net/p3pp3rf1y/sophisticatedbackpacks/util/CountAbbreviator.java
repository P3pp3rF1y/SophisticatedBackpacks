package net.p3pp3rf1y.sophisticatedbackpacks.util;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

public class CountAbbreviator {
	private CountAbbreviator() {}

	private static final NavigableMap<Integer, String> COUNT_SUFFIXES = new TreeMap<>();
	private static final DecimalFormat TWO_DIGIT_PRECISION;
	private static final DecimalFormat ONE_DIGIT_PRECISION;

	static {
		COUNT_SUFFIXES.put(1_000, "k");
		COUNT_SUFFIXES.put(1_000_000, "m");
		COUNT_SUFFIXES.put(1_000_000_000, "b");

		TWO_DIGIT_PRECISION = new DecimalFormat("#.00");
		TWO_DIGIT_PRECISION.setRoundingMode(RoundingMode.DOWN);
		ONE_DIGIT_PRECISION = new DecimalFormat("##.0");
		ONE_DIGIT_PRECISION.setRoundingMode(RoundingMode.DOWN);
	}

	public static String abbreviate(int count) {
		if (count < 1000) {
			return Integer.toString(count);
		}

		Map.Entry<Integer, String> e = COUNT_SUFFIXES.floorEntry(count);
		Integer divideBy = e.getKey();
		String suffix = e.getValue();

		String numberPart;
		if (count < 10 * divideBy) {
			numberPart = TWO_DIGIT_PRECISION.format((double) count / divideBy);
		} else if (count < 100 * divideBy) {
			numberPart = ONE_DIGIT_PRECISION.format((double) count / divideBy);
		} else {
			numberPart = Integer.toString(count / divideBy);
		}
		return numberPart + suffix;
	}

}
