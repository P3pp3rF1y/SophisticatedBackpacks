package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraftforge.items.IItemHandlerModifiable;

import java.util.function.IntConsumer;

public interface IObservableItemHandler extends IItemHandlerModifiable {
	void addListener(IntConsumer onContentsChanged);

	void clearListeners();
}
