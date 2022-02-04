package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.tree.LiteralCommandNode;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.synchronization.ArgumentTypes;
import net.minecraft.commands.synchronization.EmptyArgumentSerializer;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class SBPCommand {
	private static final int OP_LEVEL = 2;

	private SBPCommand() {}

	public static void register(CommandDispatcher<CommandSourceStack> dispatcher) {
		LiteralCommandNode<CommandSourceStack> mainNode = dispatcher.register(
				Commands.literal("sbp")
						.requires(cs -> cs.hasPermission(OP_LEVEL))
						.then(ListCommand.register())
						.then(GiveCommand.register())
						.then(RemoveNonPlayerCommand.register())
		);
		dispatcher.register(Commands.literal("sophisticatedbackpacks").requires(cs -> cs.hasPermission(OP_LEVEL)).redirect(mainNode));
	}

	public static void registerArgumentTypes() {
		ArgumentTypes.register(SophisticatedBackpacks.getRegistryName("backpack_uuid"), BackpackUUIDArgumentType.class, new EmptyArgumentSerializer<>(BackpackUUIDArgumentType::backpackUuid));
		ArgumentTypes.register(SophisticatedBackpacks.getRegistryName("player_name"), BackpackPlayerArgumentType.class, new EmptyArgumentSerializer<>(BackpackPlayerArgumentType::playerName));
	}
}
