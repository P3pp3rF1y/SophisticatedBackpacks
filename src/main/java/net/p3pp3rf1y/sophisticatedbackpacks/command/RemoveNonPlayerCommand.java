package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;

public class RemoveNonPlayerCommand {
	private RemoveNonPlayerCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSource, ?> register() {
		return Commands.literal("removeNonPlayer")
				.then(Commands.argument("onlyWithEmptyInventory", BoolArgumentType.bool())
						.executes(context -> removeNonPlayerBackpacks(context.getSource(), BoolArgumentType.getBool(context, "onlyWithEmptyInventory"))));
	}

	private static int removeNonPlayerBackpacks(CommandSource source, boolean onlyWithEmptyInventory) {
		int numberRemoved = BackpackStorage.get().removeNonPlayerBackpackContents(onlyWithEmptyInventory);
		source.sendSuccess(new TranslationTextComponent("commands.sophisticatedbackpacks.remove_non_player.success", numberRemoved), false);
		return 0;
	}
}
