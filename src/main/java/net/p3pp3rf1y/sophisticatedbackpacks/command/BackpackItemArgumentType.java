package net.p3pp3rf1y.sophisticatedbackpacks.command;

import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.arguments.item.ItemArgument;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;

public class BackpackItemArgumentType extends ItemArgument {
	private static final Collection<String> EXAMPLES = Arrays.asList("backpack", "sophisticatedbackpacks:backpack", "sophisticatedbackpacks:backpack{foo=bar}");

	static CommandBuildContext backpackContext(final HolderLookup.Provider provider) {
		return new CommandBuildContext() {
			@Override
			public Stream<ResourceKey<? extends Registry<?>>> listRegistries() {
				return provider.listRegistries();
			}

			@Override
			public <T> Optional<HolderLookup.RegistryLookup<T>> lookup(ResourceKey<? extends Registry<? extends T>> resourceKey) {
				return provider.lookup(resourceKey).map(lookup -> lookup.filterElements(item -> item instanceof BackpackItem));
			}
		};
	}

	public BackpackItemArgumentType(CommandBuildContext context) {
		super(backpackContext(context));
	}

	public static BackpackItemArgumentType item(CommandBuildContext context) {
		return new BackpackItemArgumentType(context);
	}

	public Collection<String> getExamples() {
		return EXAMPLES;
	}
}
