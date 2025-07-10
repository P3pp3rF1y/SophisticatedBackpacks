package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.*;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.AccessLogRecord;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

public class ListCommand {
	private ListCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSourceStack, ?> register() {
		return Commands.literal("list")
				.executes(context -> printBackpackList(new ArrayList<>(BackpackAccessLogger.getAllBackpackLogs()), context.getSource()))
				.then(Commands.argument("playerName", BackpackPlayerArgumentType.playerName())
						.executes(context -> printBackpackList(new ArrayList<>(BackpackAccessLogger.getBackpackLogsForPlayer(context.getArgument("playerName", String.class))), context.getSource()))
				);
	}

	private static int printBackpackList(List<AccessLogRecord> allLogs, CommandSourceStack source) {
		SimpleDateFormat dateFormat = new SimpleDateFormat();
		allLogs.sort(Comparator.comparing(AccessLogRecord::accessTime).reversed());
		source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.list.header"), false);
		allLogs.forEach(alr -> {
			MutableComponent message = Component.literal("");
			message.append(Component.literal(alr.backpackName())
					.withStyle(s ->
							s.withColor(ChatFormatting.GREEN).withClickEvent(new ClickEvent.SuggestCommand("/sophisticatedbackpacks give @p " + alr.backpackUuid()))
									.withHoverEvent(new HoverEvent.ShowText(Component.translatable("chat.sophisticatedbackpacks.backpack_uuid.tooltip", alr.backpackUuid().toString())))
					)
			);
			message.append(Component.literal(", "));
			MutableComponent clothColor = Component.translatable("commands.sophisticatedbackpacks.list.cloth_color");
			clothColor.withStyle(clothColor.getStyle().withColor(TextColor.fromRgb(alr.clothColor())));
			message.append(clothColor);
			message.append(Component.literal(" "));
			MutableComponent trimColor = Component.translatable("commands.sophisticatedbackpacks.list.trim_color");
			trimColor.withStyle(trimColor.getStyle().withColor(TextColor.fromRgb(alr.trimColor())));
			message.append(trimColor);
			message.append(Component.literal(", "));
			message.append(Component.literal(alr.playerName()));
			message.append(Component.literal(", "));
			message.append(Component.literal(dateFormat.format(new Date(alr.accessTime()))));
			source.sendSuccess(() -> message, false);
		});
		return 0;
	}
}
