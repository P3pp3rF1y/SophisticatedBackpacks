package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import net.minecraftforge.registries.ForgeRegistryEntry;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

import java.util.function.Function;
import java.util.function.Predicate;

public class ModMatcher<T extends ForgeRegistryEntry<?>, C> implements Predicate<C> {
	private final String modId;
	private final Function<C, T> getObjectFromContext;

	public ModMatcher(String modId, Function<C, T> getObjectFromContext) {
		this.modId = modId;
		this.getObjectFromContext = getObjectFromContext;
		ToolRegistry.addModWithMapping(modId);
	}

	@Override
	public boolean test(C context) {
		return RegistryHelper.getRegistryName(getObjectFromContext.apply(context)).map(rn -> rn.getNamespace().equals(modId)).orElse(false);
	}
}
